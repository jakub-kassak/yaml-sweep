-- | Pass 1: IO & caching — load all YAML files recursively.
module Yaml.Sweep.Loader
  ( YamlCache,
    loadAndCache,
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
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.YAML (Doc (..), Mapping, Node (..), Pos (..), Scalar (..), decodeNode)
import Data.YAML.Event (tagToText)
import System.Directory (doesFileExist)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import Yaml.Sweep.Types (Err, noPos)

------------------------------------------------------------------------------
-- Cache types
------------------------------------------------------------------------------

type YamlCache = Map FilePath (Node Pos)

data LoadStatus = InProgress | Done (Node Pos)

type CacheState = Map.Map FilePath LoadStatus

type LoadMonad = ExceptT Err (StateT CacheState IO)

------------------------------------------------------------------------------
-- File loading
------------------------------------------------------------------------------

loadAndCache :: FilePath -> IO (Either Err YamlCache)
loadAndCache rootPath = evalStateT (runExceptT (processFile [] rootPath)) Map.empty

processFile :: [StackFrame] -> FilePath -> LoadMonad YamlCache
processFile stack path = do
  cache <- get
  case Map.lookup path cache of
    Just InProgress -> do
      let (refPos, refFile) = case stack of
            (f : _) -> (sfPos f, sfFile f)
            [] -> (noPos, path)
          cycleStr = formatCycle stack path
      throwError (cycleStr, refPos, refFile)
    Just (Done _) -> pure Map.empty
    Nothing -> loadNewFile stack path

loadNewFile :: [StackFrame] -> FilePath -> LoadMonad YamlCache
loadNewFile stack path = do
  modify' (Map.insert path InProgress)
  exists <- liftIO $ doesFileExist path
  unless exists $ throwError (formatFileNotFoundError stack path)
  content <- liftIO $ BL.fromStrict <$> readFileBS path
  node <- case decodeNode content of
    Left (pos, err) -> throwError ([i|YAML parse error: #{err}|], pos, path)
    Right [] -> throwError ([i|empty YAML document|], noPos, path)
    Right (doc : _) -> pure (docRoot doc)
  let baseDir = takeDirectory path
      refs = nubBy ((==) `on` refPath) (findRefs baseDir node)
  subCaches <- forM refs $ \ref -> do
    let frame = StackFrame {sfFile = path, sfPos = refPos ref}
    processFile (frame : stack) (refPath ref)
  modify' (Map.insert path (Done node))
  pure $ Map.unions (Map.singleton path node : subCaches)

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
    | tagToText tag == Just "!include" -> [ConfigRef (resolvePath baseDir (T.unpack text)) pos]
  Mapping pos tag mapping
    | tagToText tag == Just "!inherit" ->
        let basePaths = maybeToList ((\t -> ConfigRef (resolvePath baseDir (T.unpack t)) pos) <$> lookupScalar mapping "base")
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
  | isAbsolute p = p
  | otherwise = baseDir </> p
