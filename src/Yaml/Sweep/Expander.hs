module Yaml.Sweep.Expander
  ( resolveAndExpand,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.String.Interpolate qualified as I
import Data.Text qualified as T
import Data.YAML (Node (..), Pos (..))
import Data.YAML.Event (Tag, mkTag)
import Yaml.Sweep.Types
  ( ConfigExpr (..),
    Err,
    LocalScopeId (..),
    ScopeEntry (..),
    ScopeEnv (..),
    ScopeIdentity (..),
    VariableIdentity (..),
    emptyLocalScopeEnv,
    emptyScopeEnv,
    explicitLocalScopeEnv,
    incScopeDepth,
    scopeInsert,
    scopeInsertGlobal,
    scopeLookup,
    scopeLookupGlobal,
    scopeLookupLocal,
    scopeMember,
    scopeName,
  )

yamlTagMap :: Tag
yamlTagMap = mkTag "tag:yaml.org,2002:map"

yamlTagSeq :: Tag
yamlTagSeq = mkTag "tag:yaml.org,2002:seq"

noPos :: Pos
noPos = Pos {posByteOffset = 0, posCharOffset = 0, posLine = 1, posColumn = 0}

data SweepValue
  = SVScalar (Node Pos)
  | SVObject Pos (M.Map (Node ()) (Pos, SweepValue))
  | SVArray Pos (IM.IntMap SweepValue)
  deriving (Show, Eq)

data HNode = HNode
  { hnHoles :: S.Set VariableIdentity,
    hnValue :: HValue
  }

data HValue
  = HVHole VariableIdentity
  | HVObject Pos (M.Map (Node ()) (Pos, HNode))
  | HVArray Pos (Maybe ScopeEnv) (IM.IntMap HNode)
  | HVConcrete SweepValue

data SweepVariant a
  = Single a
  | Expand (M.Map ScopeIdentity [a])
  deriving (Show, Functor)

type Expanded = SweepVariant HNode

data ActiveScope = ActiveScope
  { asIdentity :: ScopeIdentity,
    asIndex :: Int,
    asArity :: Int
  }
  deriving (Eq)

data Resolved a = Resolved
  { rrScopes :: [ActiveScope],
    rrValue :: a
  }

hConcrete :: SweepValue -> HNode
hConcrete value = HNode {hnHoles = S.empty, hnValue = HVConcrete value}

hHole :: VariableIdentity -> HNode
hHole variableIdentity = HNode {hnHoles = S.singleton variableIdentity, hnValue = HVHole variableIdentity}

hObject :: Pos -> M.Map (Node ()) (Pos, HNode) -> HNode
hObject pos fields =
  let hnHoles = foldMap (\(_, node) -> node.hnHoles) fields
      hnValue = maybe (HVObject pos fields) HVConcrete (concreteObject pos fields)
   in HNode {..}

hArray :: Maybe ScopeEnv -> Pos -> IM.IntMap HNode -> HNode
hArray env pos values =
  let hnHoles = foldMap (.hnHoles) values
      hnValue = maybe (HVArray pos env values) HVConcrete (concreteArray pos values)
   in HNode {..}

concreteObject :: Pos -> M.Map (Node ()) (Pos, HNode) -> Maybe SweepValue
concreteObject pos fields = SVObject pos <$> traverse (traverse concreteNode) fields

concreteArray :: Pos -> IM.IntMap HNode -> Maybe SweepValue
concreteArray pos values = SVArray pos <$> traverse concreteNode values

concreteNode :: HNode -> Maybe SweepValue
concreteNode HNode {..} =
  case hnValue of
    HVHole {} -> Nothing
    HVObject pos fields -> concreteObject pos fields
    HVArray pos _ values -> concreteArray pos values
    HVConcrete value -> Just value

mergeHNode :: HNode -> HNode -> HNode
mergeHNode left right =
  case (objectFields left, objectFields right, arrayValues left, arrayValues right) of
    (Just (leftPos, leftFields), Just (_, rightFields), _, _) ->
      hObject leftPos (M.unionWith mergeField leftFields rightFields)
    (_, _, Just (leftPos, leftEnv, leftValues), Just (_, rightEnv, rightValues)) ->
      hArray (leftEnv <|> rightEnv) leftPos (IM.unionWith mergeHNode leftValues rightValues)
    _ -> right
  where
    mergeField (_, leftValue) (rightPos, rightValue) = (rightPos, mergeHNode leftValue rightValue)

objectFields :: HNode -> Maybe (Pos, M.Map (Node ()) (Pos, HNode))
objectFields HNode {..} =
  case hnValue of
    HVObject pos fields -> Just (pos, fields)
    HVConcrete (SVObject pos fields) -> Just (pos, fmap (fmap hConcrete) fields)
    _ -> Nothing

arrayValues :: HNode -> Maybe (Pos, Maybe ScopeEnv, IM.IntMap HNode)
arrayValues HNode {..} =
  case hnValue of
    HVArray pos mEnv values -> Just (pos, mEnv, values)
    HVConcrete (SVArray pos values) -> Just (pos, Nothing, hConcrete <$> values)
    _ -> Nothing

data RawBinding = RawBinding
  { rbIdentity :: VariableIdentity,
    rbExpr :: ConfigExpr,
    rbPos :: Pos,
    rbFile :: FilePath
  }

data Binding = Binding
  { vbIdentity :: VariableIdentity,
    vbExpr :: ConfigExpr,
    vbPos :: Pos,
    vbFile :: FilePath,
    vbEnv :: ScopeEnv
  }

resolveAndExpand :: ConfigExpr -> Either Err [Node Pos]
resolveAndExpand (CEOrigin rootFile root) = resolveAndExpandAt rootFile root
resolveAndExpand root = resolveAndExpandAt "" root

resolveAndExpandAt :: FilePath -> ConfigExpr -> Either Err [Node Pos]
resolveAndExpandAt rootFile root = do
  documents <- collectDocuments rootFile root
  rawBindings <- collectBindings documents
  documentEnvs <- traverseWithKeyM (\file expr -> preScan (emptyScopeEnv file) expr) documents
  let bindings = buildBindings documentEnvs rawBindings
  rootEnv <- documentEnvs M.!? rootFile & noteMain rootFile
  vars <- execStateT (traverse_ (validateBinding bindings) (M.elems bindings)) M.empty
  rootExpanded <- evalStateT (expand bindings [] rootEnv root) vars
  expanded <- rootExpanded & fmap (fmap wrapTopArray) & noteTopArray
  resolved <- resolveFlat vars expanded
  concat <$> traverse topArray resolved
  where
    noteMain file = maybe (Left ([I.i|Main file not in declarations: #{file}|], noPos, file)) Right
    validateBinding bindings Binding {..} = prepare bindings [] vbIdentity vbPos vbFile
    noteTopArray = maybe (Left ([I.i|sweep expansion produced no top-level array|], noPos, "")) Right

wrapTopArray :: HNode -> HNode
wrapTopArray = hArray Nothing noPos . IM.singleton 0

traverseWithKeyM :: (Ord k, Monad m) => (k -> a -> m b) -> M.Map k a -> m (M.Map k b)
traverseWithKeyM f = M.foldlWithKey' step (pure M.empty)
  where
    step acc key value = do
      result <- acc
      next <- f key value
      pure (M.insert key next result)

collectDocuments :: FilePath -> ConfigExpr -> Either Err (M.Map FilePath ConfigExpr)
collectDocuments rootFile root = go S.empty rootFile root
  where
    go visited _ (CEOrigin source inner) = go visited source inner
    go visited file expr
      | file `S.member` visited = Right M.empty
      | otherwise = do
          children <- childDocuments (S.insert file visited) expr
          pure (M.insert file expr children)

    childDocuments visited = \case
      CEScalar {} -> Right M.empty
      CEOrigin file inner -> go visited file inner
      CEVarRef {} -> Right M.empty
      CEKeyDecl {} -> Right M.empty
      CEGlobalKeyDecl {} -> Right M.empty
      CEVarDecl _ _ _ value -> childDocuments visited value
      CEObject _ fields -> foldDocuments visited (snd <$> M.elems fields)
      CEArray _ values -> foldDocuments visited values
      CEZip _ _ values -> foldDocuments visited values
      CEFileBarrier _ file inner -> go visited file inner
      CEInherit _ file base override -> do
        baseDocuments <- go visited file base
        overrideDocuments <- childDocuments (S.insert file visited) override
        pure (baseDocuments <> overrideDocuments)

    foldDocuments visited = foldM step M.empty
      where
        step acc expr = (acc <>) <$> childDocuments visited expr

collectBindings :: M.Map FilePath ConfigExpr -> Either Err (M.Map VariableIdentity RawBinding)
collectBindings documents = do
  bindings <- foldM addDocument M.empty (M.toList documents)
  validateCollisions bindings
  pure bindings
  where
    addDocument bindings (file, expr) = foldM (addBinding file) bindings (documentDeclarations expr)

    addBinding file bindings (isGlobal, name, pos, value) =
      let rbIdentity = if isGlobal then GlobalVariable name else LocalVariable file name
          rawBinding = RawBinding {rbExpr = value, rbPos = pos, rbFile = file, ..}
       in case bindings M.!? rbIdentity of
            Just previous -> Left ([I.i|duplicate variable declaration: '#{name}'|], rbPos previous, rbFile previous)
            Nothing -> Right (M.insert rbIdentity rawBinding bindings)

    validateCollisions bindings =
      let localNames = S.fromList [name | LocalVariable _ name <- M.keys bindings]
          globalNames = S.fromList [name | GlobalVariable name <- M.keys bindings]
       in case S.lookupMin (S.intersection localNames globalNames) of
            Nothing -> Right ()
            Just name ->
              let matching = [binding | binding@RawBinding {rbIdentity = GlobalVariable globalName} <- M.elems bindings, globalName == name]
               in case matching of
                    binding : _ -> Left ([I.i|local and global variable name collision: '#{name}'|], rbPos binding, rbFile binding)
                    [] -> Right ()

documentDeclarations :: ConfigExpr -> [(Bool, Text, Pos, ConfigExpr)]
documentDeclarations = \case
  CEOrigin _ inner -> documentDeclarations inner
  CEObject _ fields -> fields & M.elems & map snd & mapMaybe declaration
  CEInherit _ _ _ override -> documentDeclarations override
  _ -> []
  where
    declaration = \case
      CEVarDecl isGlobal name pos value -> Just (isGlobal, name, pos, value)
      _ -> Nothing

buildBindings :: M.Map FilePath ScopeEnv -> M.Map VariableIdentity RawBinding -> M.Map VariableIdentity Binding
buildBindings environments = M.map build
  where
    build RawBinding {..} =
      let vbIdentity = rbIdentity
          vbExpr = rbExpr
          vbPos = rbPos
          vbFile = rbFile
          vbEnv = environments M.! rbFile
       in Binding {..}

preScan :: ScopeEnv -> ConfigExpr -> Either Err ScopeEnv
preScan env = \case
  CEScalar {} -> Right env
  CEOrigin _ inner -> preScan env inner
  CEVarRef {} -> Right env
  CEFileBarrier {} -> Right env
  CEInherit _ _ _ override -> preScan env override
  CEArray _ _ -> Right env
  CEVarDecl _ _ _ value -> preScan (explicitLocalScopeEnv env) value $> env
  CEKeyDecl name pos ->
    case scopeLookupLocal name env of
      Just entry
        | seDepth entry < depth env -> Left ([I.i|shadowing: '#{name}' was declared in an outer scope|], sePos entry, seFile entry)
        | seIsExplicit entry -> Left ([I.i|double definition: '#{name}' was already declared at the same scope|], sePos entry, seFile entry)
      _ -> Right (scopeInsert name (newScopeEntry (localScopeIdentity name env) True pos env) env)
  CEGlobalKeyDecl name pos ->
    case scopeLookupGlobal name env of
      Just entry -> Left ([I.i|double definition: '#{name}' was already declared at the same scope|], sePos entry, seFile entry)
      Nothing -> Right (scopeInsertGlobal name (newScopeEntry (GlobalScope name) True pos env) env)
  CEObject _ fields -> do
    let declarations = snd <$> M.elems fields
        globals = filter isGlobalKey declarations
        remaining = filter (not . isGlobalKey) declarations
    globalEnv <- foldM preScan env globals
    foldM preScan globalEnv remaining
  CEZip pos name values -> do
    traverse_ (preScan (emptyLocalScopeEnv env)) values
    pure (insertScopeIfMissing name (newScopeEntry (implicitScopeIdentity name pos env) False pos env) env)
  where
    isGlobalKey CEGlobalKeyDecl {} = True
    isGlobalKey _ = False

newScopeEntry :: ScopeIdentity -> Bool -> Pos -> ScopeEnv -> ScopeEntry
newScopeEntry seIdentity seIsExplicit sePos ScopeEnv {..} = ScopeEntry {..}
  where
    seFile = path
    seDepth = depth

insertScopeIfMissing :: Text -> ScopeEntry -> ScopeEnv -> ScopeEnv
insertScopeIfMissing name entry env = maybe (scopeInsert name entry env) (const env) (scopeLookup name env)

localScopeIdentity :: Text -> ScopeEnv -> ScopeIdentity
localScopeIdentity name ScopeEnv {..} = LocalScope (DocumentScope path name)

implicitScopeIdentity :: Text -> Pos -> ScopeEnv -> ScopeIdentity
implicitScopeIdentity name Pos {..} ScopeEnv {..} =
  LocalScope (LexicalScope (name <> ":" <> T.pack path <> ":" <> show posLine <> ":" <> show posColumn))

type Prepare = StateT (M.Map VariableIdentity Expanded) (Either Err)

expand :: M.Map VariableIdentity Binding -> [VariableIdentity] -> ScopeEnv -> ConfigExpr -> Prepare (Maybe Expanded)
expand bindings stack env = \case
  CEScalar node -> pure (Just (Single (hConcrete (SVScalar node))))
  CEOrigin _ inner -> expand bindings stack env inner
  CEVarRef name pos -> do
    variableIdentity <- lift (resolveVariable bindings env name pos)
    void (prepare bindings stack variableIdentity pos (path env))
    pure (Just (Single (hHole variableIdentity)))
  CEKeyDecl {} -> pure Nothing
  CEGlobalKeyDecl {} -> pure Nothing
  CEVarDecl {} -> pure Nothing
  CEObject pos fields -> do
    values <- traverse (traverse (expand bindings stack env)) fields
    let objects = values & M.mapMaybeWithKey objectValue & M.elems
    Just <$> lift (foldM (merge pos) (Single (hObject pos M.empty)) objects)
    where
      objectValue key (fieldPos, value) = fmap (fmap (wrapField key fieldPos)) value
      wrapField key fieldPos = hObject pos . M.singleton key . (fieldPos,)
  CEArray pos values -> do
    items <- traverse expandElement values
    localItems <- items & catMaybes & traverse (lift . expandLocalScopes env) & fmap concat
    let variants = localItems & zip [0 ..] & map arrayValue
    Just <$> lift (foldM (merge pos) (Single (hArray (Just env) pos IM.empty)) variants)
    where
      expandElement value = lift (preScan (incScopeDepth env) value) >>= \elementEnv -> expand bindings stack elementEnv value
      arrayValue (index, variant) = fmap (hArray (Just env) pos . IM.singleton index) variant
  CEZip pos name alternatives -> do
    expandedAlternatives <- traverse (expandZipAlternative bindings stack env pos) alternatives
    choices <- lift (concat <$> traverse (zipItems pos env) expandedAlternatives)
    pure (Just (Expand (M.singleton (scopeIdentity name env) choices)))
  CEFileBarrier _ file inner -> do
    includedEnv <- lift (preScan (emptyScopeEnv file) inner)
    included <- expand bindings stack includedEnv inner
    pure (namespaceScopes (IncludedScope file) <$> included)
  CEInherit pos baseFile base override -> do
    baseEnv <- lift (preScan (emptyScopeEnv baseFile) base)
    baseValue <- expand bindings stack baseEnv base
    overrideValue <- expand bindings stack env override
    case (baseValue, overrideValue) of
      (Nothing, value) -> pure value
      (Just baseConfig, Nothing) -> pure (Just (namespaceScopes (InheritedScope baseFile) baseConfig))
      (Just baseConfig, Just overrideConfig) -> Just <$> lift (merge pos (namespaceScopes (InheritedScope baseFile) baseConfig) overrideConfig)

prepare :: M.Map VariableIdentity Binding -> [VariableIdentity] -> VariableIdentity -> Pos -> FilePath -> Prepare Expanded
prepare bindings stack variableIdentity refPos refFile
  | variableIdentity `elem` stack = lift (Left ([I.i|variable cycle involving '#{variableName variableIdentity}'|], refPos, refFile))
  | otherwise = do
      prepared <- get
      case prepared M.!? variableIdentity of
        Just result -> pure result
        Nothing -> do
          binding <- lift (lookupBinding bindings variableIdentity refPos refFile)
          bindingEnv <- lift (preScan (explicitLocalScopeEnv binding.vbEnv) binding.vbExpr)
          value <- expand bindings (variableIdentity : stack) bindingEnv binding.vbExpr
          variant <- lift (maybe (Left ([I.i|variable '#{variableName variableIdentity}' has no value|], binding.vbPos, binding.vbFile)) Right value)
          modify' (M.insert variableIdentity variant)
          pure variant

resolveVariable :: M.Map VariableIdentity Binding -> ScopeEnv -> Text -> Pos -> Either Err VariableIdentity
resolveVariable bindings ScopeEnv {..} name pos =
  let localIdentity = LocalVariable path name
      global = GlobalVariable name
   in if localIdentity `M.member` bindings
        then Right localIdentity
        else
          if global `M.member` bindings
            then Right global
            else Left ([I.i|unknown variable '#{name}'|], pos, path)

lookupBinding :: M.Map VariableIdentity Binding -> VariableIdentity -> Pos -> FilePath -> Either Err Binding
lookupBinding bindings variableIdentity pos file = maybe (Left ([I.i|unknown variable '#{variableName variableIdentity}'|], pos, file)) Right (bindings M.!? variableIdentity)

variableName :: VariableIdentity -> Text
variableName = \case
  LocalVariable _ name -> name
  GlobalVariable name -> name

scopeIdentity :: Text -> ScopeEnv -> ScopeIdentity
scopeIdentity name env = maybe (localScopeIdentity name env) seIdentity (scopeLookup name env)

expandZipAlternative :: M.Map VariableIdentity Binding -> [VariableIdentity] -> ScopeEnv -> Pos -> ConfigExpr -> Prepare Expanded
expandZipAlternative bindings stack env pos value =
  expand bindings stack (emptyLocalScopeEnv env) (CEArray pos [value])
    >>= maybe (lift (Left ("zip alternative did not expand to an array", pos, path env))) pure

zipItems :: Pos -> ScopeEnv -> Expanded -> Either Err [HNode]
zipItems pos env = \case
  Single node ->
    arrayValues node
      & maybe
        (Left ("zip alternative did not expand to an array", pos, path env))
        (Right . IM.elems . third)
  Expand {} -> Left ("zip alternative did not expand to an array", pos, path env)
  where
    third (_, _, values) = values

namespaceScopes :: (LocalScopeId -> LocalScopeId) -> SweepVariant a -> SweepVariant a
namespaceScopes _ single@Single {} = single
namespaceScopes wrap (Expand scopes) = Expand (M.mapKeys namespaceScope scopes)
  where
    namespaceScope globalScope@GlobalScope {} = globalScope
    namespaceScope (LocalScope localScope@DocumentScope {}) = LocalScope localScope
    namespaceScope (LocalScope localScope) = LocalScope (wrap localScope)

merge :: Pos -> Expanded -> Expanded -> Either Err Expanded
merge _ (Single left) (Single right) = Right (Single (mergeHNode left right))
merge _ (Single left) (Expand scopes) = Right (Expand (fmap (map (mergeHNode left)) scopes))
merge _ (Expand scopes) (Single right) = Right (Expand (fmap (map (`mergeHNode` right)) scopes))
merge pos (Expand left) (Expand right) = Expand <$> foldM combine left (M.toList right)
  where
    combine scopes (scope, values) =
      case scopes M.!? scope of
        Nothing -> Right (M.insert scope values scopes)
        Just existing
          | length existing == length values -> Right (M.insert scope (zipWith mergeHNode existing values) scopes)
          | otherwise -> Left ([I.i|inconsistent lengths for scope '#{scopeName scope}'|], pos, "")

expandLocalScopes :: ScopeEnv -> Expanded -> Either Err [Expanded]
expandLocalScopes _ single@Single {} = Right [single]
expandLocalScopes env (Expand scopes)
  | M.null external = Right (Single <$> collapse localScopes)
  | M.null localScopes = Right [Expand external]
  | otherwise = Right (collapse localScopes <&> \node -> Expand (fmap (map (mergeHNode node)) external))
  where
    (external, localScopes) = M.partitionWithKey (\scope _ -> scopeMember scope env) scopes
    collapse dimensions = do
      choices <- traverse id (M.elems dimensions)
      pure (foldl' mergeHNode (hObject noPos M.empty) choices)

resolveFlat :: M.Map VariableIdentity Expanded -> Expanded -> Either Err [SweepValue]
resolveFlat vars expanded = do
  selected <- selectMainScopes expanded
  concat <$> traverse resolveSelected selected
  where
    resolveSelected selected@Resolved {..} = selected.rrValue & resolveNode vars rrScopes & fmap (map (.rrValue))

selectMainScopes :: Expanded -> Either Err [Resolved HNode]
selectMainScopes = \case
  Single node -> Right [Resolved {rrScopes = [], rrValue = node}]
  Expand scopes -> selectScopes [] (M.toAscList scopes)

selectScopes :: [ActiveScope] -> [(ScopeIdentity, [HNode])] -> Either Err [Resolved HNode]
selectScopes active dimensions = go active (hObject noPos M.empty) dimensions
  where
    go scopes node [] = Right [Resolved {rrScopes = scopes, rrValue = node}]
    go scopes node ((scope, choices) : rest) = do
      let arity = length choices
      concat <$> traverse (selectChoice arity) (zip [0 ..] choices)
      where
        selectChoice arity (index, choice) =
          go (ActiveScope {asIdentity = scope, asIndex = index, asArity = arity} : scopes) (mergeHNode node choice) rest

resolveNode :: M.Map VariableIdentity Expanded -> [ActiveScope] -> HNode -> Either Err [Resolved SweepValue]
resolveNode vars active HNode {..} =
  case hnValue of
    HVHole variableIdentity -> do
      value <- vars M.!? variableIdentity & noteVariable variableIdentity
      selected <- selectVariableScopes active value
      concat <$> traverse resolveSelected selected
    HVObject pos fields -> resolveObject vars active fields <&> map (toObject pos)
    HVArray pos mEnv values -> resolveArray vars active mEnv values <&> map (toArray pos)
    HVConcrete value -> Right [Resolved {rrScopes = active, rrValue = value}]
  where
    noteVariable variableIdentity = maybe (Left ([I.i|unresolved variable '#{variableName variableIdentity}'|], noPos, "")) Right
    resolveSelected Resolved {..} = resolveNode vars rrScopes rrValue
    toObject pos Resolved {..} = Resolved {rrValue = SVObject pos rrValue, ..}
    toArray pos Resolved {..} = Resolved {rrValue = SVArray pos (IM.fromList (zip [0 ..] rrValue)), ..}

selectVariableScopes :: [ActiveScope] -> Expanded -> Either Err [Resolved HNode]
selectVariableScopes active = \case
  Single node -> Right [Resolved {rrScopes = active, rrValue = node}]
  Expand scopes -> go [Resolved {rrScopes = active, rrValue = hObject noPos M.empty}] (M.toAscList scopes)
  where
    go selected [] = Right selected
    go selected ((scope, choices) : rest) = do
      next <- concat <$> traverse (selectScope scope choices) selected
      go next rest

    selectScope scope choices Resolved {..} =
      case find ((== scope) . asIdentity) rrScopes of
        Just ActiveScope {..}
          | asArity /= length choices -> Left ([I.i|inconsistent lengths for scope '#{scopeName scope}'|], noPos, "")
          | otherwise -> do
              choice <- choices & drop asIndex & listToMaybe & noteSelection
              pure [Resolved {rrValue = mergeHNode rrValue choice, ..}]
        Nothing ->
          pure
            ( choices
                & zip [0 ..]
                <&> \(index, choice) ->
                  let activeScope = ActiveScope {asIdentity = scope, asIndex = index, asArity = length choices}
                   in Resolved {rrScopes = activeScope : rrScopes, rrValue = mergeHNode rrValue choice}
            )
      where
        noteSelection = maybe (Left ([I.i|inconsistent lengths for scope '#{scopeName scope}'|], noPos, "")) Right

resolveObject :: M.Map VariableIdentity Expanded -> [ActiveScope] -> M.Map (Node ()) (Pos, HNode) -> Either Err [Resolved (M.Map (Node ()) (Pos, SweepValue))]
resolveObject vars active fields = foldM resolveField [Resolved {rrScopes = active, rrValue = M.empty}] (M.toAscList fields)
  where
    resolveField results (key, (fieldPos, node)) = concat <$> traverse (resolveOne key fieldPos node) results
    resolveOne key fieldPos node result@Resolved {..} =
      resolveNode vars rrScopes node
        <&> map (addField result key fieldPos)

    addField result key fieldPos child@Resolved {..} =
      Resolved {rrValue = M.insert key (fieldPos, child.rrValue) result.rrValue, ..}

resolveArray :: M.Map VariableIdentity Expanded -> [ActiveScope] -> Maybe ScopeEnv -> IM.IntMap HNode -> Either Err [Resolved [SweepValue]]
resolveArray vars active mEnv values = foldM resolveElement [Resolved {rrScopes = active, rrValue = []}] (IM.elems values)
  where
    resolveElement results node = concat <$> traverse (resolveOne node) results
    resolveOne node result@Resolved {..} = do
      children <- resolveNode vars rrScopes node
      pure (collapseChildren result children)

    collapseChildren result children =
      case mEnv of
        Nothing -> children <&> appendResult result
        Just env ->
          children
            & map (singleResult >>> retainScopes env result.rrScopes)
            & foldl' coalesce []
            <&> appendValues result

    appendResult result Resolved {..} = Resolved {rrValue = result.rrValue <> [rrValue], ..}

    singleResult Resolved {..} = Resolved {rrValue = [rrValue], ..}

    appendValues result resolved@Resolved {..} = resolved {rrValue = result.rrValue <> rrValue}

    retainScopes env original Resolved {..} =
      Resolved {rrScopes = filter (retainScope env original) rrScopes, ..}

    retainScope env original ActiveScope {..} = any (\activeScope -> activeScope.asIdentity == asIdentity) original || scopeMember asIdentity env

    coalesce results candidate@Resolved {..} =
      case break (\result -> result.rrScopes == rrScopes) results of
        (before, result : after) -> before <> (result {rrValue = result.rrValue <> rrValue} : after)
        _ -> results <> [candidate]

topArray :: SweepValue -> Either Err [Node Pos]
topArray = \case
  SVArray _ values -> Right (IM.elems values <&> sweepToNode)
  _ -> Left ([I.i|sweep expansion produced no top-level array|], noPos, "")

sweepToNode :: SweepValue -> Node Pos
sweepToNode = \case
  SVScalar value -> value
  SVObject pos fields -> Mapping pos yamlTagMap (M.fromList [(fmap (const fieldPos) key, sweepToNode value) | (key, (fieldPos, value)) <- M.toList fields])
  SVArray pos values -> Sequence pos yamlTagSeq (IM.elems values <&> sweepToNode)
