module Yaml.Sweep.Lsp.Handler
  ( handleDefinition,
    handleHover,
    lookupSnapshot,
    removeDiagnosticRoot,
    updateDiagnosticRoot,
    validateDocument,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.YAML (Pos (..))
import Language.LSP.Protocol.Types (Diagnostic, Hover (..), Location)
import Language.LSP.Protocol.Types qualified as LSP
import System.Directory (doesFileExist)
import System.FilePath (normalise)
import Yaml.Sweep.Expander (resolveAndExpand)
import Yaml.Sweep.Loader (DocSnapshot (..), YamlCache, loadAndCache, yamlDocuments)
import Yaml.Sweep.Lsp.Definition
import Yaml.Sweep.Lsp.Inherit (findInheritHoverAt)
import Yaml.Sweep.Lsp.Types
import Yaml.Sweep.Parser (nodeToExpr)
import Yaml.Sweep.Types (Err, noPos, note)

handleDefinition :: YamlCache -> FilePath -> Pos -> Maybe Location
handleDefinition cache file pos =
  findDefinitionAt (buildJumpMap cache file) file pos >>= targetLocation
  where
    targetLocation JumpTarget {..} = do
      let locFile = jtTargetFile
          locSpan = jtTargetSpan
          source = maybe "" (.dsSource) (lookupSnapshot cache jtTargetFile)
      pure (locToLspLocation source SrcLoc {..})

lookupSnapshot :: YamlCache -> FilePath -> Maybe DocSnapshot
lookupSnapshot cache file = Map.foldrWithKey match Nothing cache.yamlDocuments
  where
    canonicalFile = normalise file
    match cachedFile snapshot found
      | normalise cachedFile == canonicalFile = Just snapshot
      | otherwise = found

handleHover :: YamlCache -> FilePath -> Pos -> Maybe Hover
handleHover cache file pos = findInheritHoverAt cache file pos <&> toHover
  where
    toHover text = Hover {..}
      where
        _contents = LSP.InL (LSP.mkMarkdown text)
        _range = Nothing

validateDocument :: (FilePath -> IO (Maybe Text)) -> FilePath -> IO (Map FilePath [Diagnostic])
validateDocument fileReader file =
  loadAndCache fileReader file
    >>= either (diagnosticsForError fileReader file) (validateCache fileReader file)

updateDiagnosticRoot ::
  LSP.NormalizedUri ->
  Map LSP.NormalizedUri [Diagnostic] ->
  Map LSP.NormalizedUri (Map LSP.NormalizedUri [Diagnostic]) ->
  (Map LSP.NormalizedUri (Map LSP.NormalizedUri [Diagnostic]), [(LSP.NormalizedUri, [Diagnostic])])
updateDiagnosticRoot rootNormUri newTargetMap diagnosticState = (updatedState, aggregates)
  where
    oldTargetMap = Map.findWithDefault Map.empty rootNormUri diagnosticState
    updatedState = Map.insert rootNormUri newTargetMap diagnosticState
    affectedTargets = Set.toList (Set.fromList (Map.keys oldTargetMap ++ Map.keys newTargetMap ++ [rootNormUri]))
    aggregates = aggregateDiagnostics affectedTargets updatedState

removeDiagnosticRoot ::
  LSP.NormalizedUri ->
  Map LSP.NormalizedUri (Map LSP.NormalizedUri [Diagnostic]) ->
  (Map LSP.NormalizedUri (Map LSP.NormalizedUri [Diagnostic]), [(LSP.NormalizedUri, [Diagnostic])])
removeDiagnosticRoot rootNormUri diagnosticState = (updatedState, aggregates)
  where
    oldTargetMap = Map.findWithDefault Map.empty rootNormUri diagnosticState
    updatedState = Map.delete rootNormUri diagnosticState
    affectedTargets = Set.toList (Set.fromList (Map.keys oldTargetMap ++ [rootNormUri]))
    aggregates = aggregateDiagnostics affectedTargets updatedState

aggregateDiagnostics ::
  [LSP.NormalizedUri] ->
  Map LSP.NormalizedUri (Map LSP.NormalizedUri [Diagnostic]) ->
  [(LSP.NormalizedUri, [Diagnostic])]
aggregateDiagnostics targets diagnosticState =
  [ (target, concat [diags | targetMap <- Map.elems diagnosticState, Just diags <- [Map.lookup target targetMap]])
  | target <- targets
  ]

validateCache :: (FilePath -> IO (Maybe Text)) -> FilePath -> YamlCache -> IO (Map FilePath [Diagnostic])
validateCache fileReader file cache =
  case validationResult of
    Left err -> diagnosticsForError fileReader file err
    Right _ -> pure Map.empty
  where
    validationResult = do
      DocSnapshot {..} <- note ("Main file not in cache", noPos, file) (lookupSnapshot cache file)
      nodeToExpr cache file dsNode >>= resolveAndExpand

diagnosticsForError :: (FilePath -> IO (Maybe Text)) -> FilePath -> Err -> IO (Map FilePath [Diagnostic])
diagnosticsForError fileReader rootFile (message, pos, errFile) = do
  source <- readSource fileReader diagnosticFile
  pure (Map.singleton diagnosticFile [makeDiagnostic source pos message])
  where
    diagnosticFile
      | null errFile = rootFile
      | otherwise = errFile

readSource :: (FilePath -> IO (Maybe Text)) -> FilePath -> IO Text
readSource fileReader file = fileReader file >>= maybe readDisk pure
  where
    readDisk = do
      exists <- doesFileExist file
      if exists
        then readFileBS file <&> fromRight "" . TE.decodeUtf8'
        else pure ""

makeDiagnostic :: Text -> Pos -> Text -> Diagnostic
makeDiagnostic source pos message = LSP.Diagnostic {..}
  where
    _range = diagnosticRange source pos
    _severity = Just LSP.DiagnosticSeverity_Error
    _code = Nothing
    _codeDescription = Nothing
    _source = Just "yaml-sweep"
    _message = message
    _tags = Nothing
    _relatedInformation = Nothing
    _data_ = Nothing

diagnosticRange :: Text -> Pos -> LSP.Range
diagnosticRange source pos@Pos {..} = LSP.Range {..}
  where
    _start = posToLspPosition source pos
    _end = posToLspPosition source endPos
    endPos = pos {posColumn = posColumn + diagnosticWidth}
    diagnosticWidth =
      lineAt source posLine
        >>= T.uncons
          . T.drop (max 0 posColumn)
          & maybe 0 (const 1)
