module Yaml.Sweep.Lsp.Inherit
  ( ProvenanceNode (..),
    ProvValue (..),
    diffInherit,
    diffInheritTrees,
    buildInheritDiffs,
    formatInheritHover,
    formatFieldHover,
    findInheritHoverAt,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.YAML (Mapping, Node (..), Pos (..), Scalar (..))
import Data.YAML.Event (tagToText)
import System.FilePath (makeRelative, normalise, takeDirectory)
import Yaml.Sweep.Loader (DocSnapshot (..), YamlCache, nodeAnn, resolvePath, scalarText, yamlDocuments)
import Yaml.Sweep.Lsp.Definition (findTagSpanNear, scalarSpanAt)
import Yaml.Sweep.Lsp.Types
import Yaml.Sweep.Types (Path, PathStep (..))

data ProvenanceNode = PNode
  { pnFile :: FilePath,
    pnPos :: Pos,
    pnVal :: ProvValue
  }
  deriving (Show, Eq)

data ProvValue
  = PScalar Scalar
  | PMapping (Map (Node ()) (Pos, ProvenanceNode))
  | PSequence [ProvenanceNode]
  deriving (Show, Eq)

data NodeSite = NodeSite
  { nsNode :: ProvenanceNode,
    nsFieldPos :: Pos
  }

data MappingField = MappingField
  { mfKey :: Node Pos,
    mfValue :: Node Pos
  }

data InheritRoot = InheritRoot
  { irBasePath :: FilePath,
    irNode :: Node Pos,
    irInheritPos :: Pos,
    irBaseKeyPos :: Maybe Pos,
    irBaseValuePos :: Maybe Pos
  }

data CachedDocument = CachedDocument
  { cdFile :: FilePath,
    cdSnapshot :: DocSnapshot
  }

diffInherit :: FilePath -> FilePath -> FilePath -> Node Pos -> Node Pos -> InheritDiff
diffInherit basePath baseFile overrideFile baseNode overrideNode =
  (diffInheritTrees baseFile baseNode overrideFile overrideNode) {idBasePath = basePath}

diffInheritTrees :: FilePath -> Node Pos -> FilePath -> Node Pos -> InheritDiff
diffInheritTrees idBaseFile baseNode idOverrideFile overrideNode =
  diffProvenance root idBaseFile idOverrideFile baseTree overrideTree
  where
    root = inheritRoot idBaseFile overrideNode
    baseTree = provenanceNode idBaseFile baseNode
    overrideTree = provenanceNode idOverrideFile root.irNode

buildInheritDiffs :: YamlCache -> FilePath -> [InheritDiff]
buildInheritDiffs cache file = maybe [] buildDocument (lookupDocument cache file)
  where
    buildDocument CachedDocument {..} = walkNode cdFile cdSnapshot.dsNode
    walkNode overrideFile node = inheritAt overrideFile node <> childInherits overrideFile node
    inheritAt overrideFile node@(Mapping _ tag _)
      | tagToText tag == Just "!inherit",
        root@InheritRoot {..} <- inheritRoot overrideFile node,
        Just baseDocument <- lookupDocument cache (resolveReference overrideFile irBasePath),
        Just baseTree <- effectiveProvenance cache baseDocument.cdFile baseDocument.cdSnapshot.dsNode,
        Just overrideTree <- localProvenance cache overrideFile root =
          [diffProvenance root baseDocument.cdFile overrideFile baseTree overrideTree]
    inheritAt _ _ = []
    childInherits overrideFile = \case
      Scalar {} -> []
      Mapping _ _ mapping -> Map.foldMapWithKey (\key value -> walkNode overrideFile key <> walkNode overrideFile value) mapping
      Sequence _ _ nodes -> foldMap (walkNode overrideFile) nodes
      Anchor _ _ inner -> walkNode overrideFile inner

formatInheritHover :: InheritDiff -> Text
formatInheritHover InheritDiff {..} =
  T.intercalate "\n\n" (header : catMaybes sections)
  where
    header = "### Inherit from " <> codeSpan (T.pack idBasePath)
    fields = Map.elems idFields
    sections =
      [ formatCategory "Overridden" (filter isOverridden fields),
        formatCategory "Added" (filter isAdded fields),
        formatCategory "Inherited" (filter isInherited fields)
      ]
    formatCategory _ [] = Nothing
    formatCategory title categoryFields =
      Just ("**" <> title <> "**\n" <> T.intercalate "\n" (formatSummary <$> categoryFields))

formatFieldHover :: InheritFieldDiff -> Text
formatFieldHover = \case
  FieldOverridden {..} ->
    "Overrides with "
      <> codeSpan ifdOverrideVal
      <> " (was "
      <> codeSpan ifdBaseVal
      <> " at "
      <> formatLocation ifdOverrideFile ifdBaseFile ifdBasePos
      <> ")"
  FieldAdded {..} -> "Adds " <> codeSpan (fieldText False ifdPath ifdOverrideVal)
  FieldInherited {..} ->
    "Inherits "
      <> codeSpan (fieldText False ifdPath ifdBaseVal)
      <> " from "
      <> formatLocation ifdOverrideFile ifdBaseFile ifdBasePos

findInheritHoverAt :: YamlCache -> FilePath -> Pos -> Maybe Text
findInheritHoverAt cache file pos = do
  CachedDocument {cdSnapshot = DocSnapshot {..}, ..} <- lookupDocument cache file
  let diffs = buildInheritDiffs cache cdFile
  asum (hoverForDiff dsSource pos <$> diffs)

effectiveProvenance :: YamlCache -> FilePath -> Node Pos -> Maybe ProvenanceNode
effectiveProvenance cache file = \case
  Scalar pos scalar -> Just (scalarProvenance file pos scalar)
  Mapping pos tag mapping
    | tagToText tag == Just "!inherit" -> effectiveInherit cache file pos mapping
    | otherwise -> mappingProvenance cache file pos mapping
  Sequence pos _ nodes -> do
    sequenceNodes <- traverse (effectiveProvenance cache file) nodes
    let pnFile = file
        pnPos = pos
        pnVal = PSequence sequenceNodes
    pure PNode {..}
  Anchor _ _ inner -> effectiveProvenance cache file inner

effectiveInherit :: YamlCache -> FilePath -> Pos -> Mapping Pos -> Maybe ProvenanceNode
effectiveInherit cache file pos mapping = do
  baseField <- findBaseField mapping
  baseText <- scalarText baseField.mfValue
  baseDocument <- lookupDocument cache (resolveReference file (T.unpack baseText))
  baseTree <- effectiveProvenance cache baseDocument.cdFile baseDocument.cdSnapshot.dsNode
  overrideTree <- mappingProvenance cache file pos (withoutBase mapping)
  pure (mergeProvenance baseTree overrideTree)

localProvenance :: YamlCache -> FilePath -> InheritRoot -> Maybe ProvenanceNode
localProvenance cache file InheritRoot {..} = case irNode of
  Mapping pos _ mapping -> mappingProvenance cache file pos mapping
  node -> effectiveProvenance cache file node

mappingProvenance :: YamlCache -> FilePath -> Pos -> Mapping Pos -> Maybe ProvenanceNode
mappingProvenance cache file pos mapping = do
  fields <- traverse provenanceField (Map.toList mapping)
  let pnFile = file
      pnPos = pos
      pnVal = PMapping (Map.fromList fields)
  pure PNode {..}
  where
    provenanceField (key, value) = do
      valueTree <- effectiveProvenance cache file value
      pure (void key, (nodeAnn key, valueTree))

provenanceNode :: FilePath -> Node Pos -> ProvenanceNode
provenanceNode file = \case
  Scalar pos scalar -> scalarProvenance file pos scalar
  Mapping pos _ mapping ->
    let pnFile = file
        pnPos = pos
        pnVal = PMapping (Map.fromList (fieldProvenance <$> Map.toList mapping))
     in PNode {..}
    where
      fieldProvenance (key, value) = (void key, (nodeAnn key, provenanceNode file value))
  Sequence pos _ nodes ->
    let pnFile = file
        pnPos = pos
        pnVal = PSequence (provenanceNode file <$> nodes)
     in PNode {..}
  Anchor _ _ inner -> provenanceNode file inner

scalarProvenance :: FilePath -> Pos -> Scalar -> ProvenanceNode
scalarProvenance pnFile pnPos scalar = PNode {..}
  where
    pnVal = PScalar scalar

mergeProvenance :: ProvenanceNode -> ProvenanceNode -> ProvenanceNode
mergeProvenance base override = case (base.pnVal, override.pnVal) of
  (PMapping baseFields, PMapping overrideFields) ->
    override {pnVal = PMapping (Map.unionWith mergeField baseFields overrideFields)}
  _ -> override
  where
    mergeField (_, baseNode) (overridePos, overrideNode) =
      (overridePos, mergeProvenance baseNode overrideNode)

withoutBase :: Mapping Pos -> Mapping Pos
withoutBase = Map.filterWithKey (\key _ -> scalarText key /= Just "base")

diffProvenance :: InheritRoot -> FilePath -> FilePath -> ProvenanceNode -> ProvenanceNode -> InheritDiff
diffProvenance InheritRoot {..} idBaseFile idOverrideFile baseTree overrideTree = InheritDiff {..}
  where
    idBasePath = irBasePath
    idInheritPos = irInheritPos
    idBaseKeyPos = irBaseKeyPos
    idBaseValuePos = irBaseValuePos
    baseSite = nodeSite baseTree
    overrideSite = nodeSite overrideTree
    idFields = diffSites idOverrideFile [] baseSite overrideSite

diffSites :: FilePath -> Path -> NodeSite -> NodeSite -> Map.Map Path InheritFieldDiff
diffSites overrideFile path baseSite overrideSite = case (baseSite.nsNode.pnVal, overrideSite.nsNode.pnVal) of
  (PMapping baseMapping, PMapping overrideMapping) ->
    diffMappings overrideFile path baseMapping overrideMapping
  _ -> overriddenField path baseSite overrideSite

diffMappings :: FilePath -> Path -> Map.Map (Node ()) (Pos, ProvenanceNode) -> Map.Map (Node ()) (Pos, ProvenanceNode) -> Map.Map Path InheritFieldDiff
diffMappings overrideFile path baseMapping overrideMapping = baseDiffs <> addedDiffs
  where
    baseDiffs = Map.foldMapWithKey compareBaseField baseMapping
    addedDiffs = Map.foldMapWithKey addOverrideField (Map.difference overrideMapping baseMapping)
    compareBaseField key baseField =
      let fieldPath = path <> [pathStep key]
          baseSite = fieldSite baseField
       in case Map.lookup key overrideMapping of
            Just overrideField -> diffSites overrideFile fieldPath baseSite (fieldSite overrideField)
            Nothing -> inheritedLeaves overrideFile fieldPath baseSite
    addOverrideField key overrideField =
      addedLeaves overrideFile (path <> [pathStep key]) (fieldSite overrideField)

inheritedLeaves :: FilePath -> Path -> NodeSite -> Map.Map Path InheritFieldDiff
inheritedLeaves overrideFile path site = case site.nsNode.pnVal of
  PMapping mapping
    | not (Map.null mapping) -> Map.foldMapWithKey inheritField mapping
  PSequence nodes
    | not (null nodes) -> inheritNodes 0 nodes
  _ -> inheritedField overrideFile path site
  where
    inheritField key field = inheritedLeaves overrideFile (path <> [pathStep key]) (fieldSite field)
    inheritIndex index node = inheritedLeaves overrideFile (path <> [PSIdx index]) (nodeSite node)
    inheritNodes _ [] = Map.empty
    inheritNodes !index (node : nodes) = inheritIndex index node <> inheritNodes (index + 1) nodes

addedLeaves :: FilePath -> Path -> NodeSite -> Map.Map Path InheritFieldDiff
addedLeaves overrideFile path site = case site.nsNode.pnVal of
  PMapping mapping
    | not (Map.null mapping) -> Map.foldMapWithKey addField mapping
  PSequence nodes
    | not (null nodes) -> addNodes 0 nodes
  _ -> addedField overrideFile path site
  where
    addField key field = addedLeaves overrideFile (path <> [pathStep key]) (fieldSite field)
    addIndex index node = addedLeaves overrideFile (path <> [PSIdx index]) (nodeSite node)
    addNodes _ [] = Map.empty
    addNodes !index (node : nodes) = addIndex index node <> addNodes (index + 1) nodes

overriddenField :: Path -> NodeSite -> NodeSite -> Map.Map Path InheritFieldDiff
overriddenField ifdPath baseSite overrideSite = Map.singleton ifdPath FieldOverridden {..}
  where
    ifdBaseFile = baseSite.nsNode.pnFile
    ifdBasePos = baseSite.nsFieldPos
    ifdBaseVal = renderProvenance baseSite.nsNode
    ifdOverrideFile = overrideSite.nsNode.pnFile
    ifdOverridePos = overrideSite.nsFieldPos
    ifdOverrideValuePos = overrideSite.nsNode.pnPos
    ifdOverrideVal = renderProvenance overrideSite.nsNode

inheritedField :: FilePath -> Path -> NodeSite -> Map.Map Path InheritFieldDiff
inheritedField ifdOverrideFile ifdPath site = Map.singleton ifdPath FieldInherited {..}
  where
    ifdBaseFile = site.nsNode.pnFile
    ifdBasePos = site.nsFieldPos
    ifdBaseVal = renderProvenance site.nsNode

addedField :: FilePath -> Path -> NodeSite -> Map.Map Path InheritFieldDiff
addedField ifdOverrideFile ifdPath site = Map.singleton ifdPath FieldAdded {..}
  where
    ifdOverridePos = site.nsFieldPos
    ifdOverrideValuePos = site.nsNode.pnPos
    ifdOverrideVal = renderProvenance site.nsNode

inheritRoot :: FilePath -> Node Pos -> InheritRoot
inheritRoot fallbackPath (Mapping pos tag mapping)
  | tagToText tag == Just "!inherit" = InheritRoot {..}
  where
    baseField = findBaseField mapping
    irBasePath = maybe fallbackPath (T.unpack . fromMaybe "" . scalarText . (.mfValue)) baseField
    irNode = Mapping pos tag (withoutBase mapping)
    irInheritPos = pos
    irBaseKeyPos = nodeAnn . (.mfKey) <$> baseField
    irBaseValuePos = nodeAnn . (.mfValue) <$> baseField
inheritRoot fallbackPath node = InheritRoot {..}
  where
    irBasePath = fallbackPath
    irNode = node
    irInheritPos = nodeAnn node
    irBaseKeyPos = Nothing
    irBaseValuePos = Nothing

findBaseField :: Mapping Pos -> Maybe MappingField
findBaseField = Map.foldrWithKey match Nothing
  where
    match key value found
      | scalarText key == Just "base" =
          let mfKey = key
              mfValue = value
           in Just MappingField {..}
      | otherwise = found

fieldSite :: (Pos, ProvenanceNode) -> NodeSite
fieldSite (nsFieldPos, nsNode) = NodeSite {..}

nodeSite :: ProvenanceNode -> NodeSite
nodeSite nsNode = NodeSite {nsFieldPos = nsNode.pnPos, ..}

pathStep :: Node () -> PathStep
pathStep = PSKey . renderNode

renderProvenance :: ProvenanceNode -> Text
renderProvenance PNode {..} = case pnVal of
  PScalar scalar -> renderScalar scalar
  PMapping mapping ->
    "{" <> T.intercalate ", " (Map.elems (Map.mapWithKey renderField mapping)) <> "}"
  PSequence nodes -> "[" <> T.intercalate ", " (renderProvenance <$> nodes) <> "]"
  where
    renderField key (_, value) = renderNode key <> ": " <> renderProvenance value

renderNode :: Node a -> Text
renderNode = \case
  Scalar _ scalar -> renderScalar scalar
  Mapping _ _ mapping ->
    "{" <> T.intercalate ", " (Map.elems (Map.mapWithKey (\key value -> renderNode key <> ": " <> renderNode value) mapping)) <> "}"
  Sequence _ _ nodes -> "[" <> T.intercalate ", " (renderNode <$> nodes) <> "]"
  Anchor _ _ inner -> renderNode inner

renderScalar :: Scalar -> Text
renderScalar = \case
  SNull -> "null"
  SBool value -> if value then "true" else "false"
  SFloat value -> T.pack (show value)
  SInt value -> T.pack (show value)
  SStr value -> value
  SUnknown tag value -> maybe value (\tagText -> tagText <> " " <> value) (tagToText tag)

formatSummary :: InheritFieldDiff -> Text
formatSummary field = case field of
  FieldOverridden {..} ->
    "- "
      <> codeSpan (fieldText True ifdPath ifdOverrideVal)
      <> " (was "
      <> codeSpan ifdBaseVal
      <> " at "
      <> formatLocation ifdOverrideFile ifdBaseFile ifdBasePos
      <> ")"
  FieldAdded {..} -> "- " <> codeSpan (fieldText True ifdPath ifdOverrideVal)
  FieldInherited {..} ->
    "- "
      <> codeSpan (fieldText True ifdPath ifdBaseVal)
      <> " ("
      <> formatLocation ifdOverrideFile ifdBaseFile ifdBasePos
      <> ")"

fieldText :: Bool -> Path -> Text -> Text
fieldText fullPath path value = fieldName <> ": " <> value
  where
    fieldName
      | fullPath = formatPath path
      | otherwise = formatLeaf path

formatPath :: Path -> Text
formatPath = foldl' appendStep ""
  where
    appendStep prefix (PSKey key)
      | T.null prefix = key
      | otherwise = prefix <> "." <> key
    appendStep prefix (PSIdx index) = prefix <> "[" <> T.pack (show index) <> "]"

formatLeaf :: Path -> Text
formatLeaf path = case reverse path of
  PSKey key : _ -> key
  PSIdx index : _ -> "[" <> T.pack (show index) <> "]"
  [] -> "value"

formatLocation :: FilePath -> FilePath -> Pos -> Text
formatLocation contextFile sourceFile Pos {..} =
  T.pack displayFile <> ":" <> T.pack (show posLine) <> ":" <> T.pack (show (posColumn + 1))
  where
    displayFile = makeRelative (takeDirectory contextFile) sourceFile

hoverForDiff :: Text -> Pos -> InheritDiff -> Maybe Text
hoverForDiff source pos diff@InheritDiff {..} = fieldHover <|> rootHover
  where
    fieldHover = find (fieldContains source pos) (Map.elems idFields) <&> formatFieldHover
    rootHover
      | isInheritTag source pos diff || isBaseReference source pos diff = Just (formatInheritHover diff)
      | otherwise = Nothing

fieldContains :: Text -> Pos -> InheritFieldDiff -> Bool
fieldContains source pos = \case
  FieldOverridden {..} -> containsField source pos ifdOverridePos ifdOverrideValuePos ifdOverrideVal
  FieldAdded {..} -> containsField source pos ifdOverridePos ifdOverrideValuePos ifdOverrideVal
  FieldInherited {} -> False

containsField :: Text -> Pos -> Pos -> Pos -> Text -> Bool
containsField source pos fieldPos valuePos value = any (`spanContainsPos` pos) spans
  where
    spans = catMaybes [lineSpanFrom source fieldPos, lineSpanFrom source valuePos, scalarSpanAt source valuePos value]

lineSpanFrom :: Text -> Pos -> Maybe SrcSpan
lineSpanFrom source Pos {..} = do
  line <- sourceLine source posLine
  let spanStartLine = posLine
      spanStartCol = posColumn
      spanEndLine = posLine
      spanEndCol = T.length line
  pure SrcSpan {..}

sourceLine :: Text -> Int -> Maybe Text
sourceLine source lineNumber
  | lineNumber < 1 = Nothing
  | otherwise = listToMaybe (drop (lineNumber - 1) (T.splitOn "\n" source))

isInheritTag :: Text -> Pos -> InheritDiff -> Bool
isInheritTag source pos InheritDiff {..} =
  maybe False (`spanContainsPos` pos) (findTagSpanNear source idInheritPos "!inherit")

isBaseReference :: Text -> Pos -> InheritDiff -> Bool
isBaseReference source pos InheritDiff {..} = any contains spans
  where
    spans =
      [ idBaseKeyPos >>= \keyPos -> includeFollowingColumn <$> scalarSpanAt source keyPos "base",
        idBaseValuePos >>= \valuePos -> scalarSpanAt source valuePos (T.pack idBasePath)
      ]
    contains = maybe False (`spanContainsPos` pos)

lookupDocument :: YamlCache -> FilePath -> Maybe CachedDocument
lookupDocument cache file = Map.foldrWithKey match Nothing cache.yamlDocuments
  where
    canonicalFile = normalise file
    match cachedFile cdSnapshot found
      | normalise cachedFile == canonicalFile = Just CachedDocument {cdFile = cachedFile, ..}
      | otherwise = found

resolveReference :: FilePath -> FilePath -> FilePath
resolveReference file = normalise . resolvePath (takeDirectory file)

includeFollowingColumn :: SrcSpan -> SrcSpan
includeFollowingColumn sourceSpan@SrcSpan {..} = sourceSpan {spanEndCol = spanEndCol + 1}

isOverridden :: InheritFieldDiff -> Bool
isOverridden FieldOverridden {} = True
isOverridden _ = False

isAdded :: InheritFieldDiff -> Bool
isAdded FieldAdded {} = True
isAdded _ = False

isInherited :: InheritFieldDiff -> Bool
isInherited FieldInherited {} = True
isInherited _ = False

codeSpan :: Text -> Text
codeSpan text = delimiter <> text <> delimiter
  where
    delimiter = T.replicate (longestBacktickRun text + 1) "`"

longestBacktickRun :: Text -> Int
longestBacktickRun = go 0 0
  where
    go !current !longest remaining = case T.uncons remaining of
      Nothing -> max current longest
      Just ('`', rest) -> go (current + 1) longest rest
      Just (_, rest) -> go 0 (max current longest) rest
