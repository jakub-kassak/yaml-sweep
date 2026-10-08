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
import Yaml.Sweep.Loader (DocSnapshot (..), YamlCache, resolvePath, scalarText)
import Yaml.Sweep.Lsp.Types
import Yaml.Sweep.Types

buildJumpMap :: YamlCache -> FilePath -> JumpMap
buildJumpMap cache rootFile = rootJumps <> otherJumps
  where
    canonicalRoot = normalise rootFile
    canonicalCache = Map.foldlWithKey' insertCanonical Map.empty cache
    insertCanonical acc file snapshot = Map.insert (normalise file) snapshot acc
    variables = collectVariables canonicalCache
    rootJumps = maybe Map.empty (buildDocument variables canonicalRoot) (Map.lookup canonicalRoot canonicalCache)
    otherJumps = Map.foldMapWithKey (buildDocument variables) (Map.delete canonicalRoot canonicalCache)

buildDocument :: VariableBindings -> FilePath -> DocSnapshot -> JumpMap
buildDocument variables dcFile DocSnapshot {..} = walkNode context documentEnv dsNode <> variableJumps variables context dsNode
  where
    context = DocumentContext {dcSource = dsSource, ..}
    documentEnv = preScanNode True (incScopeDepth (emptyScopeEnv dcFile)) dsNode

data DocumentContext = DocumentContext
  { dcFile :: FilePath,
    dcSource :: Text
  }

data VariableEntry = VariableEntry
  { veIdentity :: VariableIdentity,
    veFile :: FilePath,
    veSource :: Text,
    vePos :: Pos
  }

newtype VariableBindings = VariableBindings
  { unVariableBindings :: Map VariableIdentity [VariableEntry]
  }

collectVariables :: YamlCache -> VariableBindings
collectVariables = VariableBindings . Map.fromListWith (<>) . concatMap documentVariables . Map.toList

documentVariables :: (FilePath, DocSnapshot) -> [(VariableIdentity, [VariableEntry])]
documentVariables (file, DocSnapshot {..}) = do
  mapping <- maybeToList (rootMapping dsNode)
  (key, _) <- Map.toList mapping
  (isGlobal, name, pos) <- maybeToList (variableDeclaration key)
  let veIdentity = if isGlobal then GlobalVariable name else LocalVariable file name
      veFile = file
      veSource = dsSource
      vePos = pos
      entry = VariableEntry {..}
  pure (veIdentity, [entry])

rootMapping :: Node Pos -> Maybe (Mapping Pos)
rootMapping = \case
  Mapping _ _ mapping -> Just mapping
  Anchor _ _ inner -> rootMapping inner
  _ -> Nothing

variableDeclaration :: Node Pos -> Maybe (Bool, Text, Pos)
variableDeclaration (Scalar pos (SUnknown tag name))
  | tagToText tag == Just "!set", not (T.null name) = Just (False, name, pos)
  | tagToText tag == Just "!global_set", not (T.null name) = Just (True, name, pos)
variableDeclaration _ = Nothing

preScanNode :: Bool -> ScopeEnv -> Node Pos -> ScopeEnv
preScanNode isDocumentRoot env = \case
  Scalar {} -> env
  Mapping _ tag _
    | tagToText tag == Just "!inherit" -> env
  Mapping _ _ mapping ->
    let (globalDeclarations, otherDeclarations) = Map.partitionWithKey (\key _ -> isGlobalKey key) mapping
        globalEnv = Map.foldlWithKey' (preScanValue isDocumentRoot) env globalDeclarations
     in Map.foldlWithKey' (preScanValue isDocumentRoot) globalEnv otherDeclarations
  Sequence pos tag nodes
    | Just name <- tagToText tag >>= T.stripPrefix "!zip_" ->
        foldl' (preScanNode False) (insertImplicit name pos env) nodes
    | tagToText tag == Just "!prod" -> foldl' (preScanNode False) env nodes
    | otherwise -> env
  Anchor _ _ inner -> preScanNode isDocumentRoot env inner
  where
    isGlobalKey key = case keyDeclaration key of
      Just KeyDeclaration {kdKind = GlobalDeclaration} -> True
      _ -> False

    preScanValue atRoot acc key value = case keyDeclaration key of
      Just KeyDeclaration {kdKind = GlobalDeclaration, ..}
        | atRoot -> insertGlobal kdName kdPos acc
      Just KeyDeclaration {kdKind = LocalDeclaration, ..} -> insertExplicit kdName kdPos acc
      _ -> preScanNode False acc value

walkNode :: DocumentContext -> ScopeEnv -> Node Pos -> JumpMap
walkNode context@DocumentContext {..} env = \case
  Scalar pos (SUnknown tag path)
    | tagToText tag == Just "!include" -> includeJump context pos path
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
    elementEnv = preScanNode False (incScopeDepth env) node

variableJumps :: VariableBindings -> DocumentContext -> Node Pos -> JumpMap
variableJumps bindings context = \case
  Scalar pos (SUnknown tag _)
    | Just name <- tagToText tag >>= T.stripPrefix "!var_",
      not (T.null name) ->
        variableJump bindings context pos name
  Scalar {} -> Map.empty
  Mapping _ tag mapping
    | tagToText tag == Just "!inherit" ->
        foldMap (variableJumps bindings context) (Map.elems (Map.filterWithKey (\key _ -> scalarText key /= Just "base") mapping))
    | otherwise -> foldMap (variableJumps bindings context) (Map.elems mapping)
  Sequence _ _ nodes -> foldMap (variableJumps bindings context) nodes
  Anchor _ _ inner -> variableJumps bindings context inner

variableJump :: VariableBindings -> DocumentContext -> Pos -> Text -> JumpMap
variableJump bindings DocumentContext {..} pos name =
  fromMaybe Map.empty do
    sourceSpan <- findTagSpanNear dcSource pos tagText
    VariableEntry {..} <- resolveVariable bindings dcFile name
    targetSpan <- scalarSpanAt veSource vePos name
    pure (singletonJump dcFile sourceSpan veFile targetSpan)
  where
    tagText = "!var_" <> name

resolveVariable :: VariableBindings -> FilePath -> Text -> Maybe VariableEntry
resolveVariable VariableBindings {..} file name = case (localEntries, globalEntries) of
  ([entry], []) -> Just entry
  ([], [entry])
    | not (hasLocalName name) -> Just entry
  _ -> Nothing
  where
    localEntries = Map.findWithDefault [] (LocalVariable file name) unVariableBindings
    globalEntries = Map.findWithDefault [] (GlobalVariable name) unVariableBindings
    hasLocalName target = any isMatchingLocal (Map.keys unVariableBindings)
      where
        isMatchingLocal = \case
          LocalVariable _ localName -> localName == target
          GlobalVariable {} -> False

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
    kdPos :: Pos,
    kdKind :: DeclarationKind
  }

data DeclarationKind = LocalDeclaration | GlobalDeclaration

keyDeclaration :: Node Pos -> Maybe KeyDeclaration
keyDeclaration (Scalar pos (SUnknown tag name))
  | tagToText tag == Just "!key" =
      let kdName = name
          kdKind = LocalDeclaration
          kdPos = pos
       in Just KeyDeclaration {..}
keyDeclaration (Scalar pos (SUnknown tag name))
  | tagToText tag == Just "!global_key" =
      let kdName = name
          kdPos = pos
       in Just KeyDeclaration {kdKind = GlobalDeclaration, ..}
keyDeclaration _ = Nothing

isKeyDeclaration :: Node Pos -> Bool
isKeyDeclaration = isJust . keyDeclaration

insertExplicit :: Text -> Pos -> ScopeEnv -> ScopeEnv
insertExplicit name pos env@ScopeEnv {..} =
  case scopeLookupLocal name env of
    Just ScopeEntry {..}
      | seDepth < depth || seIsExplicit -> env
    _ -> scopeInsert name entry env
  where
    entry = scopeEntry (LocalScope (LexicalScope name)) True pos env

insertGlobal :: Text -> Pos -> ScopeEnv -> ScopeEnv
insertGlobal name pos env = maybe (scopeInsertGlobal name entry env) (const env) (scopeLookupGlobal name env)
  where
    entry = scopeEntry (GlobalScope name) True pos env

insertImplicit :: Text -> Pos -> ScopeEnv -> ScopeEnv
insertImplicit name pos env
  | isJust (scopeLookup name env) = env
  | otherwise = scopeInsert name (scopeEntry (LocalScope (LexicalScope name)) False pos env) env

scopeEntry :: ScopeIdentity -> Bool -> Pos -> ScopeEnv -> ScopeEntry
scopeEntry seIdentity seIsExplicit sePos ScopeEnv {..} = ScopeEntry {..}
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
