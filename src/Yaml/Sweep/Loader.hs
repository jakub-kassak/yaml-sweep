-- | Pass 1: IO & caching — load all YAML files recursively.
module Yaml.Sweep.Loader
  ( DocSnapshot (..),
    collapseParentSegments,
    YamlCache,
    loadReferencedYamlDocuments,
    loadReferencedYamlDocumentsWith,
    findRefs,
    resolvePath,
    scalarText,
    lookupScalar,
    nodeAnn,
    formatSource,
  )
where

import Control.Monad.Except (throwError)
import Data.ByteString.Lazy qualified as BL
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.YAML (Doc (..), Mapping, Node (..), Pos (..), Scalar (..), decodeNode)
import Data.YAML.Event (tagToText)
import System.FilePath (isAbsolute, joinPath, normalise, splitDirectories, takeDirectory, (</>))
import Yaml.Sweep.Source (SourceReadError (..), SourceReader, readYamlFile)
import Yaml.Sweep.Types (Err, noPos)

------------------------------------------------------------------------------
-- Cache types
------------------------------------------------------------------------------

data DocSnapshot = DocSnapshot
  { dsSource :: Text,
    dsNode :: Node Pos
  }

type YamlCache = Map FilePath DocSnapshot

data CacheState = CacheState
  { csInProgress :: Set FilePath,
    csDocuments :: YamlCache
  }

type LoadMonad = ReaderT SourceReader (ExceptT Err (StateT CacheState IO))

------------------------------------------------------------------------------
-- File loading
------------------------------------------------------------------------------

loadReferencedYamlDocuments :: FilePath -> IO (Either Err YamlCache)
loadReferencedYamlDocuments = loadReferencedYamlDocumentsWith readYamlFile

loadReferencedYamlDocumentsWith :: SourceReader -> FilePath -> IO (Either Err YamlCache)
loadReferencedYamlDocumentsWith sourceReader rootPath =
  evalStateT (runExceptT $ runReaderT loadCache sourceReader) CacheState {csInProgress = Set.empty, csDocuments = Map.empty}
  where
    loadCache = do
      processFile [] rootPath
      csDocuments <$> get

processFile :: [StackFrame] -> FilePath -> LoadMonad ()
processFile stack rawPath = do
  let path = collapseParentSegments rawPath
  CacheState {..} <- get
  when (path `Set.member` csInProgress) $
    err path
  unless (path `Map.member` csDocuments) $
    loadNewFile stack path
  where
    err path =
      let (refPos, refFile) = case stack of
            (f : _) -> (sfPos f, sfFile f)
            [] -> (noPos, path)
          cycleStr = formatCycle stack path
       in throwError (cycleStr, refPos, refFile)

loadNewFile :: [StackFrame] -> FilePath -> LoadMonad ()
loadNewFile stack path = do
  modify' $ \cacheState@CacheState {..} -> cacheState {csInProgress = Set.insert path csInProgress}
  sourceReader <- ask
  source <- liftIO (sourceReader path) >>= either (throwError . formatSourceReadError stack path) pure
  let content = BL.fromStrict (TE.encodeUtf8 source)
  node <- case decodeNode content of
    Left (pos, err) -> throwError ([i|YAML parse error: #{err}|], pos, path)
    Right [] -> throwError ([i|empty YAML document|], noPos, path)
    Right (doc : _) -> pure (docRoot doc)
  let baseDir = takeDirectory path
      refs = nubBy ((==) `on` refPath) (findRefs baseDir node)
  forM_ refs $ \ref -> do
    let frame = StackFrame {sfFile = path, sfPos = refPos ref}
    processFile (frame : stack) (refPath ref)
  let dsSource = source
      dsNode = node
      snapshot = DocSnapshot {..}
  modify' $ \cacheState@CacheState {..} -> cacheState {csInProgress = Set.delete path csInProgress, csDocuments = Map.insert path snapshot csDocuments}

------------------------------------------------------------------------------
-- Reference finding
------------------------------------------------------------------------------

data StackFrame = StackFrame {sfFile :: FilePath, sfPos :: Pos}
  deriving (Show, Eq)

data ConfigRef = ConfigRef {refPath :: FilePath, refPos :: Pos}
  deriving (Show, Eq)

findRefs :: FilePath -> Node Pos -> [ConfigRef]
findRefs baseDir = \case
  Scalar pos (SUnknown tag text)
    | tagToText tag == Just "!include" ->
        let refPath = resolvePath baseDir (T.unpack text)
            refPos = pos
         in [ConfigRef {..}]
  Mapping pos tag mapping
    | tagToText tag == Just "!inherit" ->
        let basePaths = maybeToList do
              t <- lookupScalar mapping "base"
              let refPath = resolvePath baseDir (T.unpack t)
                  refPos = pos
              pure ConfigRef {..}
            childRefs = concatMap (\(k, v) -> findRefs baseDir k <> findRefs baseDir v) (Map.toList mapping)
         in basePaths <> childRefs
  Mapping _ _ mapping ->
    concatMap (\(k, v) -> findRefs baseDir k <> findRefs baseDir v) (Map.toList mapping)
  Sequence _ _ nodes -> concatMap (findRefs baseDir) nodes
  Anchor _ _ inner -> findRefs baseDir inner
  _ -> []

------------------------------------------------------------------------------
-- Error formatting
------------------------------------------------------------------------------

formatSource :: FilePath -> Pos -> String
formatSource file pos = [i|#{file}:#{posLine pos}:#{posColumn pos}|]

formatCycle :: [StackFrame] -> FilePath -> Text
formatCycle stack path =
  let chain = reverse (path : map sfFile stack)
      chainStr = T.intercalate " -> " (map toText chain)
   in [i|Cycle detected: #{chainStr}|]

formatSourceReadError :: [StackFrame] -> FilePath -> SourceReadError -> Err
formatSourceReadError stack path SourceNotFound = formatFileNotFoundError stack path
formatSourceReadError _ path (SourceUtf8Error message) = ([i|UTF-8 decode error: #{message}|], noPos, path)

formatFileNotFoundError :: [StackFrame] -> FilePath -> Err
formatFileNotFoundError [] path = ([i|file not found: #{path}|], noPos, path)
formatFileNotFoundError (frame : rest) path =
  let refChain = formatStack (frame : rest)
   in ([i|file not found: #{path}\nReferenced in:\n#{refChain}|], sfPos frame, sfFile frame)

formatStack :: [StackFrame] -> String
formatStack [] = ""
formatStack (frame : rest) =
  let pos = sfPos frame
      file = sfFile frame
      thisFrame = [i|  - #{formatSource file pos}|]
   in case rest of
        [] -> thisFrame
        _ -> thisFrame <> "\n" <> formatStack rest

------------------------------------------------------------------------------
-- Shared helpers (re-exported for Parser)
------------------------------------------------------------------------------

-- | get Node annotation
nodeAnn :: Node a -> a
nodeAnn = \case
  Scalar a _ -> a
  Mapping a _ _ -> a
  Sequence a _ _ -> a
  Anchor a _ _ -> a

scalarText :: Node Pos -> Maybe Text
scalarText (Scalar _ (SStr t)) = Just t
scalarText (Scalar _ (SUnknown _ t)) = Just t
scalarText _ = Nothing

lookupScalar :: Mapping Pos -> Text -> Maybe Text
lookupScalar m key = join (listToMaybe [scalarText v | (k, v) <- Map.toList m, scalarText k == Just key])

resolvePath :: FilePath -> FilePath -> FilePath
resolvePath baseDir p
  | isAbsolute p = collapseParentSegments p
  | otherwise = collapseParentSegments (baseDir </> p)

collapseParentSegments :: FilePath -> FilePath
collapseParentSegments path =
  joinPath $ reverse $ foldl' collapse [] (splitDirectories (normalise path))
  where
    collapse parts ".." = case parts of
      part : rest
        | part /= ".." && not (isAbsolute path && null rest) -> rest
      _
        | isAbsolute path -> parts
        | otherwise -> ".." : parts
    collapse parts part = part : parts
