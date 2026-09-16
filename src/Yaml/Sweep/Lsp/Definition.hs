module Yaml.Sweep.Lsp.Definition
  ( buildJumpMap,
    findDefinitionAt,
    findTagSpanNear,
    scalarSpanAt,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.YAML (Mapping, Node (..), Pos (..), Scalar (..))
import Data.YAML.Event (tagToText)
import System.FilePath (normalise, takeDirectory)
import Yaml.Sweep.Loader (DocSnapshot (..), YamlCache, resolvePath, scalarText, yamlDocuments)
import Yaml.Sweep.Lsp.Types
import Yaml.Sweep.Types

buildJumpMap :: YamlCache -> FilePath -> JumpMap
buildJumpMap cache rootFile = rootJumps <> otherJumps
  where
    canonicalRoot = normalise rootFile
    canonicalCache = Map.foldlWithKey' insertCanonical Map.empty cache.yamlDocuments
    insertCanonical acc file snapshot = Map.insert (normalise file) snapshot acc
    rootJumps = maybe Map.empty (buildDocument canonicalRoot) (Map.lookup canonicalRoot canonicalCache)
    otherJumps = Map.foldMapWithKey buildDocument (Map.delete canonicalRoot canonicalCache)

buildDocument :: FilePath -> DocSnapshot -> JumpMap
buildDocument dcFile DocSnapshot {..} = walkNode context documentEnv dsNode
  where
    context = DocumentContext {dcSource = dsSource, ..}
    documentEnv = preScanNode (incScopeDepth (emptyScopeEnv dcFile)) dsNode

data DocumentContext = DocumentContext
  { dcFile :: FilePath,
    dcSource :: Text
  }

preScanNode :: ScopeEnv -> Node Pos -> ScopeEnv
preScanNode env = \case
  Scalar {} -> env
  Mapping _ tag _
    | tagToText tag == Just "!inherit" -> env
  Mapping _ _ mapping -> Map.foldlWithKey' preScanValue env mapping
  Sequence pos tag nodes
    | Just name <- tagToText tag >>= T.stripPrefix "!zip_" ->
        foldl' preScanNode (insertImplicit name pos env) nodes
    | tagToText tag == Just "!prod" -> foldl' preScanNode env nodes
    | otherwise -> env
  Anchor _ _ inner -> preScanNode env inner
  where
    preScanValue acc key value = maybe (preScanNode acc value) (declareExplicit acc) (keyDeclaration key)
    declareExplicit acc KeyDeclaration {..} = insertExplicit kdName kdPos acc

walkNode :: DocumentContext -> ScopeEnv -> Node Pos -> JumpMap
walkNode context@DocumentContext {..} env = \case
  Scalar pos (SUnknown tag path)
    | tagToText tag == Just "!include" -> includeJump context pos path
    | tagToText tag == Just "!load_str" -> includeJump context pos path
  Scalar {} -> Map.empty
  Mapping _ tag mapping
    | tagToText tag == Just "!inherit" ->
        inheritJump context mapping <> walkMapping context (emptyScopeEnv dcFile) mapping
  Mapping _ _ mapping -> walkMapping context env mapping
  Sequence pos tag nodes
    | Just name <- tagToText tag >>= T.stripPrefix "!zip_" ->
        zipJump context env pos name <> foldMap (walkNode context env) nodes
    | tagToText tag == Just "!prod" -> foldMap (walkNode context env) nodes
    | otherwise -> foldMap (walkElement context env) nodes
  Anchor _ _ inner -> walkNode context env inner

walkMapping :: DocumentContext -> ScopeEnv -> Mapping Pos -> JumpMap
walkMapping context env = Map.foldMapWithKey walkValue
  where
    walkValue key value
      | isKeyDeclaration key = Map.empty
      | otherwise = walkNode context env value

walkElement :: DocumentContext -> ScopeEnv -> Node Pos -> JumpMap
walkElement context env node = walkNode context elementEnv node
  where
    elementEnv = preScanNode (incScopeDepth env) node

zipJump :: DocumentContext -> ScopeEnv -> Pos -> Text -> JumpMap
zipJump DocumentContext {..} env pos name =
  fromMaybe Map.empty do
    sourceSpan <- findTagSpanNear dcSource pos tagText
    declaration <- scopeLookup name env
    guard declaration.seIsExplicit
    targetSpan <- scalarSpanAt dcSource declaration.sePos name
    pure (singletonJump dcFile sourceSpan declaration.seFile targetSpan)
  where
    tagText = "!zip_" <> name

includeJump :: DocumentContext -> Pos -> Text -> JumpMap
includeJump DocumentContext {..} pos path =
  maybe Map.empty (\sourceSpan -> singletonJump dcFile sourceSpan targetFile fileStartSpan) (scalarSpanAt dcSource pos path)
  where
    targetFile = canonicalReference dcFile path

inheritJump :: DocumentContext -> Mapping Pos -> JumpMap
inheritJump DocumentContext {..} = Map.foldMapWithKey baseJump
  where
    baseJump key value
      | scalarText key == Just "base",
        Just path <- scalarText value,
        Just sourceSpan <- scalarSpanAt dcSource (nodePosition value) path =
          singletonJump dcFile sourceSpan (canonicalReference dcFile path) fileStartSpan
      | otherwise = Map.empty

canonicalReference :: FilePath -> Text -> FilePath
canonicalReference file = normalise . resolvePath (takeDirectory file) . T.unpack

singletonJump :: FilePath -> SrcSpan -> FilePath -> SrcSpan -> JumpMap
singletonJump file sourceSpan jtTargetFile jtTargetSpan =
  Map.singleton (normalise file, sourceSpan) JumpTarget {..}

fileStartSpan :: SrcSpan
fileStartSpan = SrcSpan {..}
  where
    spanStartLine = 1
    spanStartCol = 0
    spanEndLine = 1
    spanEndCol = 0

data KeyDeclaration = KeyDeclaration
  { kdName :: Text,
    kdPos :: Pos
  }

keyDeclaration :: Node Pos -> Maybe KeyDeclaration
keyDeclaration (Scalar pos (SUnknown tag name))
  | tagToText tag == Just "!key" =
      let kdName = name
          kdPos = pos
       in Just KeyDeclaration {..}
keyDeclaration _ = Nothing

isKeyDeclaration :: Node Pos -> Bool
isKeyDeclaration = isJust . keyDeclaration

insertExplicit :: Text -> Pos -> ScopeEnv -> ScopeEnv
insertExplicit name pos env@ScopeEnv {..} =
  case scopeLookup name env of
    Just ScopeEntry {..}
      | seDepth < depth || seIsExplicit -> env
    _ -> scopeInsert name entry env
  where
    entry = scopeEntry True pos env

insertImplicit :: Text -> Pos -> ScopeEnv -> ScopeEnv
insertImplicit name pos env
  | scopeMember name env = env
  | otherwise = scopeInsert name (scopeEntry False pos env) env

scopeEntry :: Bool -> Pos -> ScopeEnv -> ScopeEntry
scopeEntry seIsExplicit sePos ScopeEnv {..} = ScopeEntry {..}
  where
    seFile = path
    seDepth = depth

nodePosition :: Node Pos -> Pos
nodePosition = \case
  Scalar pos _ -> pos
  Mapping pos _ _ -> pos
  Sequence pos _ _ -> pos
  Anchor pos _ _ -> pos

findTagSpanNear :: Text -> Pos -> Text -> Maybe SrcSpan
findTagSpanNear source Pos {..} target = findFromLine posLine
  where
    findFromLine lineNumber
      | lineNumber < 1 = Nothing
      | lineNumber == posLine = findTagSpanOnLine sourceBeforeNode lineNumber target <|> findFromLine (lineNumber - 1)
      | otherwise = findTagSpanOnLine source lineNumber target <|> findFromLine (lineNumber - 1)
    sourceBeforeNode = replaceLine source posLine (T.take posColumn)

scalarSpanAt :: Text -> Pos -> Text -> Maybe SrcSpan
scalarSpanAt source Pos {..} value = do
  line <- sourceLine source posLine
  width <- scalarTokenWidth (T.drop posColumn line) value
  pure (singleLineSpan posLine posColumn (posColumn + width))

scalarTokenWidth :: Text -> Text -> Maybe Int
scalarTokenWidth source value = case T.uncons source of
  Just ('"', rest) -> quotedWidth '"' rest <&> (+ 1)
  Just ('\'', rest) -> singleQuotedWidth rest <&> (+ 1)
  _ -> do
    guard (value `T.isPrefixOf` source)
    pure (T.length value)

quotedWidth :: Char -> Text -> Maybe Int
quotedWidth quote = go 0 False
  where
    go width escaped source = do
      (char, rest) <- T.uncons source
      if
        | escaped -> go (width + 1) False rest
        | char == '\\' -> go (width + 1) True rest
        | char == quote -> Just (width + 1)
        | otherwise -> go (width + 1) False rest

singleQuotedWidth :: Text -> Maybe Int
singleQuotedWidth = go 0
  where
    go width source = do
      (char, rest) <- T.uncons source
      if char /= '\''
        then go (width + 1) rest
        else case T.uncons rest of
          Just ('\'', afterEscape) -> go (width + 2) afterEscape
          _ -> Just (width + 1)

sourceLine :: Text -> Int -> Maybe Text
sourceLine source lineNumber
  | lineNumber < 1 = Nothing
  | otherwise = listToMaybe (drop (lineNumber - 1) (T.splitOn "\n" source))

replaceLine :: Text -> Int -> (Text -> Text) -> Text
replaceLine source lineNumber transform = T.intercalate "\n" (go lineNumber (T.splitOn "\n" source))
  where
    go _ [] = []
    go 1 (line : rest) = transform line : rest
    go remaining (line : rest) = line : go (remaining - 1) rest

singleLineSpan :: Int -> Int -> Int -> SrcSpan
singleLineSpan spanStartLine spanStartCol spanEndCol = SrcSpan {..}
  where
    spanEndLine = spanStartLine

findDefinitionAt :: JumpMap -> FilePath -> Pos -> Maybe JumpTarget
findDefinitionAt jumpMap file pos = Map.foldrWithKey findMatch Nothing jumpMap
  where
    canonicalFile = normalise file
    findMatch (sourceFile, sourceSpan) target found
      | sourceFile == canonicalFile && spanContainsPos sourceSpan pos = Just target
      | otherwise = found
