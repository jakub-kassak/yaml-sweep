module LspSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.YAML (Pos (..))
import Language.LSP.Protocol.Types (Diagnostic (..), Location, Position (..), Range (..), UInt)
import Language.LSP.Protocol.Types qualified as LSP
import System.FilePath (normalise, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Yaml.Sweep.Loader (DocSnapshot (..), YamlCache, loadAndCache, loadAndCacheSnapshots)
import Yaml.Sweep.Lsp.Definition (buildJumpMap, findDefinitionAt)
import Yaml.Sweep.Lsp.Handler (handleDefinition, handleHover, removeDiagnosticRoot, updateDiagnosticRoot, validateDocument)
import Yaml.Sweep.Lsp.Inherit (buildInheritDiffs, findInheritHoverAt, formatInheritHover)
import Yaml.Sweep.Lsp.Types
import Yaml.Sweep.Types (PathStep (..), renderErr)

data DefinitionCase = DefinitionCase
  { dcDirectory :: FilePath,
    dcSourceLine :: Int,
    dcSourceCol :: Int,
    dcSourceEndCol :: Int,
    dcTargetFile :: FilePath,
    dcTargetLine :: Int,
    dcTargetCol :: Int,
    dcTargetEndCol :: Int,
    dcDescription :: String
  }

spec :: FilePath -> Spec
spec examplesDir = describe "LSP features" do
  for_ definitionCases \definitionCase@DefinitionCase {..} ->
    it dcDescription $ withCache (configFileFor definitionCase) \cache -> do
      let configFile = configFileFor definitionCase
          jumpMap = buildJumpMap cache configFile
          expectedTarget = targetFor examplesDir definitionCase
          sourceSpan = singleLineSpan dcSourceLine dcSourceCol dcSourceEndCol
          cursor = at dcSourceLine dcSourceCol
      Map.lookup (normalise configFile, sourceSpan) jumpMap `shouldBe` Just expectedTarget
      findDefinitionAt jumpMap configFile cursor `shouldBe` Just expectedTarget
      handleDefinition cache configFile cursor `shouldBe` targetLocation cache expectedTarget
  describe "scope boundaries" do
    it "does not jump for an implicit scope" $
      withFixture implicitScopeFiles "config.yaml" \configFile cache ->
        findDefinitionAt (buildJumpMap cache configFile) configFile (at 2 10) `shouldBe` Nothing
    it "keeps implicit scopes in sibling elements isolated" $
      withFixture siblingImplicitFiles "config.yaml" \configFile cache -> do
        let jumpMap = buildJumpMap cache configFile
        findDefinitionAt jumpMap configFile (at 2 11) `shouldBe` Nothing
        findDefinitionAt jumpMap configFile (at 3 11) `shouldBe` Nothing
    it "does not link an implicit scope to a declaration in a sibling element" $
      withFixture siblingScopeFiles "config.yaml" \configFile cache -> do
        let jumpMap = buildJumpMap cache configFile
            target = jumpTarget configFile (singleLineSpan 2 9 10)
        findDefinitionAt jumpMap configFile (at 3 11) `shouldBe` Just target
        findDefinitionAt jumpMap configFile (at 4 11) `shouldBe` Nothing
    it "does not carry scopes across include boundaries" $
      withFixture includeBoundaryFiles "config.yaml" \configFile cache -> do
        let includeFile = takeDirectory configFile </> "include.yaml"
        findDefinitionAt (buildJumpMap cache configFile) includeFile (at 1 7) `shouldBe` Nothing
    it "does not carry scopes across inherit boundaries" $
      withFixture inheritBoundaryFiles "config.yaml" \configFile cache ->
        findDefinitionAt (buildJumpMap cache configFile) configFile (at 3 9) `shouldBe` Nothing
  describe "source spans" do
    it "uses exact spans for spaced and block-style tags and quoted paths" $
      withFixture exactSpanFiles "config.yaml" \configFile cache -> do
        let jumpMap = buildJumpMap cache configFile
            directory = takeDirectory configFile
            declaration = jumpTarget configFile (singleLineSpan 1 7 8)
            includeTarget = jumpTarget (directory </> "include.yaml") fileStartSpan
            baseTarget = jumpTarget (directory </> "base.yaml") fileStartSpan
        Map.lookup (configFile, singleLineSpan 2 8 14) jumpMap `shouldBe` Just declaration
        Map.lookup (configFile, singleLineSpan 3 7 13) jumpMap `shouldBe` Just declaration
        Map.lookup (configFile, singleLineSpan 6 18 32) jumpMap `shouldBe` Just includeTarget
        Map.lookup (configFile, singleLineSpan 8 8 19) jumpMap `shouldBe` Just baseTarget
    it "matches flexible whitespace in key declarations" $
      findTagSpanOnLine exactSpanSource 1 "!key a" `shouldBe` Just (singleLineSpan 1 0 8)
    it "matches only positions inside a reference token" $
      withFixture exactSpanFiles "config.yaml" \configFile cache -> do
        let jumpMap = buildJumpMap cache configFile
            expected = Just (jumpTarget configFile (singleLineSpan 1 7 8))
        findDefinitionAt jumpMap configFile (at 2 8) `shouldBe` expected
        findDefinitionAt jumpMap configFile (at 2 7) `shouldBe` Nothing
        findDefinitionAt jumpMap configFile (at 2 14) `shouldBe` Nothing
    it "converts source columns to UTF-16 code units" do
      let source = "😀 !zip_a\n"
          sourceSpan = singleLineSpan 1 2 8
      spanToLspRange source sourceSpan `shouldBe` lspRange 0 3 0 9
      posToLspPosition source (at 1 2) `shouldBe` lspPosition 0 3
      lspPositionToPos source (lspPosition 0 3) `shouldBe` at 1 2
    it "retains source text in each cache snapshot" $
      withFixture exactSpanFiles "config.yaml" \configFile cache ->
        (Map.lookup configFile cache <&> (.dsSource)) `shouldBe` Just exactSpanSource
    it "loads editor buffers instead of stale disk content" $
      withFixture [fixtureFile "config.yaml" "disk: stale\n"] "config.yaml" \configFile _ -> do
        let liveSource = "editor: live\n"
            fileReader file
              | normalise file == normalise configFile = pure (Just liveSource)
              | otherwise = pure Nothing
        loadAndCache fileReader configFile
          >>= either (expectationFailure . renderErr) (\cache -> (Map.lookup configFile cache <&> (.dsSource)) `shouldBe` Just liveSource)
  describe "diagnostics" do
    it "validates parsing and sweep expansion against editor buffers" $
      withFixture [fixtureFile "config.yaml" "value: disk\n"] "config.yaml" \configFile _ -> do
        let invalidReader file
              | normalise file == normalise configFile = pure (Just "😀: !prod []\n")
              | otherwise = pure Nothing
            validReader file
              | normalise file == normalise configFile = pure (Just "value: !prod [one]\n")
              | otherwise = pure Nothing
        invalidDiagnostics <- validateDocument invalidReader configFile
        validDiagnostics <- validateDocument validReader configFile
        case Map.lookup configFile invalidDiagnostics of
          Just [diagnostic] -> do
            diagnostic._message `shouldBe` "!prod with empty list"
            diagnostic._range `shouldBe` lspRange 0 10 0 11
          _ -> expectationFailure "expected one diagnostic for the editor buffer"
        validDiagnostics `shouldBe` Map.empty
    it "aggregates shared targets and clears only diagnostics from the closed root" do
      let rootA = normalizedUri "/root-a.yaml"
          rootB = normalizedUri "/root-b.yaml"
          sharedTarget = normalizedUri "/shared.yaml"
          diagnosticA = testDiagnostic "root A"
          diagnosticB = testDiagnostic "root B"
          (stateA, _) = updateDiagnosticRoot rootA (Map.singleton sharedTarget [diagnosticA]) Map.empty
          (stateAB, publishedAB) = updateDiagnosticRoot rootB (Map.singleton sharedTarget [diagnosticB]) stateA
          (stateB, publishedAfterACloses) = removeDiagnosticRoot rootA stateAB
          (stateEmpty, publishedAfterBCloses) = removeDiagnosticRoot rootB stateB
      Map.fromList publishedAB
        `shouldBe` Map.fromList [(rootB, []), (sharedTarget, [diagnosticA, diagnosticB])]
      Map.fromList publishedAfterACloses
        `shouldBe` Map.fromList [(rootA, []), (sharedTarget, [diagnosticB])]
      Map.fromList publishedAfterBCloses
        `shouldBe` Map.fromList [(rootB, []), (sharedTarget, [])]
      stateEmpty `shouldBe` Map.empty
  describe "!inherit inspection" do
    it "classifies nested overridden and inherited fields" $
      withCache (examplesDir </> "17-inherit" </> "config.yaml") \cache ->
        withSingleInheritDiff cache (examplesDir </> "17-inherit" </> "config.yaml") \diff -> do
          let fields = diff.idFields
          Map.lookup [PSKey "traces", PSKey "samples"] fields `shouldSatisfy` isOverriddenField
          Map.lookup [PSKey "traces", PSKey "mode"] fields `shouldSatisfy` isInheritedField
    it "formats categorized structural paths" $
      withCache (examplesDir </> "17-inherit" </> "config.yaml") \cache ->
        withSingleInheritDiff cache (examplesDir </> "17-inherit" </> "config.yaml") \diff -> do
          let hover = formatInheritHover diff
          hover `shouldSatisfy` T.isInfixOf "**Overridden**"
          hover `shouldSatisfy` T.isInfixOf "`traces.samples: 2`"
          hover `shouldSatisfy` T.isInfixOf "**Inherited**"
          hover `shouldSatisfy` T.isInfixOf "`traces.mode: post_hoc`"
    it "shows the diff over the inherit tag and base reference" $
      withCache (examplesDir </> "17-inherit" </> "config.yaml") \cache ->
        withSingleInheritDiff cache (examplesDir </> "17-inherit" </> "config.yaml") \diff -> do
          let configFile = examplesDir </> "17-inherit" </> "config.yaml"
              expected = Just (formatInheritHover diff)
          findInheritHoverAt cache configFile (at 4 5) `shouldBe` expected
          findInheritHoverAt cache configFile (at 5 1) `shouldBe` expected
          findInheritHoverAt cache configFile (at 5 4) `shouldBe` expected
          handleHover cache configFile (at 4 5) `shouldSatisfy` isJust
    it "shows base provenance over an overridden field" $
      withCache (examplesDir </> "17-inherit" </> "config.yaml") \cache ->
        findInheritHoverAt cache (examplesDir </> "17-inherit" </> "config.yaml") (at 7 3)
          `shouldBe` Just "Overrides with `2` (was `1` at inherit.yaml:5:3)"
    it "shows field hover over the key, colon, and value" $
      withCache (examplesDir </> "17-inherit" </> "config.yaml") \cache -> do
        let configFile = examplesDir </> "17-inherit" </> "config.yaml"
            expected = Just "Overrides with `2` (was `1` at inherit.yaml:5:3)"
        findInheritHoverAt cache configFile (at 7 3) `shouldBe` expected
        findInheritHoverAt cache configFile (at 7 9) `shouldBe` expected
        findInheritHoverAt cache configFile (at 7 11) `shouldBe` expected
    it "shows added-field hover text" $
      withFixture addedInheritFiles "config.yaml" \configFile cache ->
        findInheritHoverAt cache configFile (at 4 3) `shouldBe` Just "Adds `new_field: true`"
    it "retains the true field origin through an inheritance chain" $
      withFixture chainedInheritFiles "config.yaml" \configFile cache ->
        withSingleInheritDiff cache configFile \diff -> do
          let expectedBase = takeDirectory configFile </> "grand.yaml"
              middleBase = takeDirectory configFile </> "base.yaml"
          Map.lookup [PSKey "limit"] diff.idFields `shouldSatisfy` hasBaseOrigin expectedBase (at 1 0)
          Map.lookup [PSKey "untouched"] diff.idFields `shouldSatisfy` hasBaseOrigin expectedBase (at 2 0)
          Map.lookup [PSKey "middle"] diff.idFields `shouldSatisfy` hasBaseOrigin middleBase (at 3 0)
          Map.member [PSKey "base"] diff.idFields `shouldBe` False
          findInheritHoverAt cache configFile (at 3 9)
            `shouldBe` Just "Overrides with `20` (was `5` at grand.yaml:1:1)"
    it "treats a changed sequence as one replaced field" $
      withFixture sequenceInheritFiles "config.yaml" \configFile cache ->
        withSingleInheritDiff cache configFile \diff -> do
          Map.lookup [PSKey "items"] diff.idFields `shouldSatisfy` isOverriddenField
          Map.member [PSKey "items", PSIdx 0] diff.idFields `shouldBe` False
          findInheritHoverAt cache configFile (at 4 4)
            `shouldBe` Just "Overrides with `[three]` (was `[one, two]` at base.yaml:1:1)"
    it "uses delimiter-aware CommonMark code spans" $
      withFixture backtickInheritFiles "config.yaml" \configFile cache ->
        findInheritHoverAt cache configFile (at 3 12)
          `shouldBe` Just "Overrides with ``new `value`` (was ``old `value`` at base.yaml:1:1)"
  where
    configFileFor DefinitionCase {..} = examplesDir </> dcDirectory </> "config.yaml"

withCache :: FilePath -> (YamlCache -> Expectation) -> Expectation
withCache file assertion =
  loadAndCacheSnapshots file >>= either (expectationFailure . renderErr) assertion

withSingleInheritDiff :: YamlCache -> FilePath -> (InheritDiff -> Expectation) -> Expectation
withSingleInheritDiff cache file assertion = case buildInheritDiffs cache file of
  [diff] -> assertion diff
  diffs -> expectationFailure ("expected one inherit diff, got " <> show (length diffs))

targetFor :: FilePath -> DefinitionCase -> JumpTarget
targetFor examplesDir DefinitionCase {..} = JumpTarget {..}
  where
    jtTargetFile = normalise (examplesDir </> dcDirectory </> dcTargetFile)
    jtTargetSpan = singleLineSpan dcTargetLine dcTargetCol dcTargetEndCol

targetLocation :: YamlCache -> JumpTarget -> Maybe Location
targetLocation cache JumpTarget {..} = do
  DocSnapshot {..} <- Map.lookup jtTargetFile cache
  let locFile = jtTargetFile
      locSpan = jtTargetSpan
  pure (locToLspLocation dsSource SrcLoc {..})

at :: Int -> Int -> Pos
at posLine posColumn = Pos {..}
  where
    posByteOffset = 0
    posCharOffset = 0

singleLineSpan :: Int -> Int -> Int -> SrcSpan
singleLineSpan spanStartLine spanStartCol spanEndCol = SrcSpan {..}
  where
    spanEndLine = spanStartLine

fileStartSpan :: SrcSpan
fileStartSpan = singleLineSpan 1 0 0

jumpTarget :: FilePath -> SrcSpan -> JumpTarget
jumpTarget jtTargetFile jtTargetSpan = JumpTarget {..}

lspPosition :: UInt -> UInt -> Position
lspPosition _line _character = Position {..}

lspRange :: UInt -> UInt -> UInt -> UInt -> Range
lspRange startLine startCharacter endLine endCharacter = Range {..}
  where
    _start = lspPosition startLine startCharacter
    _end = lspPosition endLine endCharacter

normalizedUri :: FilePath -> LSP.NormalizedUri
normalizedUri = LSP.toNormalizedUri . LSP.filePathToUri

testDiagnostic :: Text -> Diagnostic
testDiagnostic _message = Diagnostic {..}
  where
    _range = lspRange 0 0 0 1
    _severity = Just LSP.DiagnosticSeverity_Error
    _code = Nothing
    _codeDescription = Nothing
    _source = Just "yaml-sweep"
    _tags = Nothing
    _relatedInformation = Nothing
    _data_ = Nothing

data FixtureFile = FixtureFile
  { ffPath :: FilePath,
    ffSource :: Text
  }

withFixture :: [FixtureFile] -> FilePath -> (FilePath -> YamlCache -> Expectation) -> Expectation
withFixture files rootFile assertion =
  withSystemTempDirectory "yaml-sweep-lsp" \directory -> do
    for_ files \FixtureFile {..} -> TIO.writeFile (directory </> ffPath) ffSource
    let configFile = directory </> rootFile
    withCache configFile (assertion configFile)

implicitScopeFiles :: [FixtureFile]
implicitScopeFiles =
  [fixtureFile "config.yaml" "items:\n  - left: !zip_a [1, 2]\n    right: !zip_a [3, 4]\n"]

siblingScopeFiles :: [FixtureFile]
siblingScopeFiles =
  [fixtureFile "config.yaml" "items:\n  - !key a:\n    value: !zip_a [1, 2]\n  - value: !zip_a [3, 4]\n"]

siblingImplicitFiles :: [FixtureFile]
siblingImplicitFiles =
  [fixtureFile "config.yaml" "items:\n  - value: !zip_a [1, 2]\n  - value: !zip_a [3, 4]\n"]

includeBoundaryFiles :: [FixtureFile]
includeBoundaryFiles =
  [ fixtureFile "config.yaml" "!key a:\nincluded: !include include.yaml\n",
    fixtureFile "include.yaml" "value: !zip_a [1, 2]\n"
  ]

inheritBoundaryFiles :: [FixtureFile]
inheritBoundaryFiles =
  [ fixtureFile "config.yaml" "derived: !inherit\n  base: base.yaml\n  local: !zip_a [1, 2]\n",
    fixtureFile "base.yaml" "!key a:\nvalue: !zip_a [1, 2]\n"
  ]

addedInheritFiles :: [FixtureFile]
addedInheritFiles =
  [ fixtureFile "config.yaml" "derived: !inherit\n  base: base.yaml\n  max_rounds: 8\n  new_field: true\n",
    fixtureFile "base.yaml" "max_rounds: 5\n"
  ]

chainedInheritFiles :: [FixtureFile]
chainedInheritFiles =
  [ fixtureFile "config.yaml" "derived: !inherit\n  base: base.yaml\n  limit: 20\n",
    fixtureFile "base.yaml" "--- !inherit\nbase: grand.yaml\nmiddle: true\n",
    fixtureFile "grand.yaml" "limit: 5\nuntouched: final\n"
  ]

sequenceInheritFiles :: [FixtureFile]
sequenceInheritFiles =
  [ fixtureFile "config.yaml" "derived: !inherit\n  base: base.yaml\n  items:\n    - three\n",
    fixtureFile "base.yaml" "items: [one, two]\n"
  ]

backtickInheritFiles :: [FixtureFile]
backtickInheritFiles =
  [ fixtureFile "config.yaml" "derived: !inherit\n  base: base.yaml\n  message: \"new `value\"\n",
    fixtureFile "base.yaml" "message: \"old `value\"\n"
  ]

exactSpanFiles :: [FixtureFile]
exactSpanFiles =
  [ fixtureFile "config.yaml" exactSpanSource,
    fixtureFile "include.yaml" "included: true\n",
    fixtureFile "base.yaml" "base: true\n"
  ]

fixtureFile :: FilePath -> Text -> FixtureFile
fixtureFile ffPath ffSource = FixtureFile {..}

exactSpanSource :: Text
exactSpanSource =
  "!key   a:\ninline: !zip_a   [1, 2]\nblock: !zip_a\n  - 1\n  - 2\ninclude: !include \"include.yaml\"\nderived: !inherit\n  base: \"base.yaml\"\n"

definitionCases :: [DefinitionCase]
definitionCases =
  [ DefinitionCase
      { dcDirectory = "07-zip-key-cross-element",
        dcSourceLine = 7,
        dcSourceCol = 20,
        dcSourceEndCol = 27,
        dcTargetFile = "config.yaml",
        dcTargetLine = 5,
        dcTargetCol = 5,
        dcTargetEndCol = 7,
        dcDescription = "jumps from a cross-element zip to its top-level key"
      },
    DefinitionCase
      { dcDirectory = "14-nested-two-keys",
        dcSourceLine = 11,
        dcSourceCol = 14,
        dcSourceEndCol = 20,
        dcTargetFile = "config.yaml",
        dcTargetLine = 6,
        dcTargetCol = 5,
        dcTargetEndCol = 6,
        dcDescription = "jumps from a nested zip to its outer key"
      },
    DefinitionCase
      { dcDirectory = "14-nested-two-keys",
        dcSourceLine = 13,
        dcSourceCol = 14,
        dcSourceEndCol = 20,
        dcTargetFile = "config.yaml",
        dcTargetLine = 9,
        dcTargetCol = 11,
        dcTargetEndCol = 12,
        dcDescription = "jumps from a nested zip to its enclosing key"
      },
    DefinitionCase
      { dcDirectory = "16-include",
        dcSourceLine = 3,
        dcSourceCol = 13,
        dcSourceEndCol = 25,
        dcTargetFile = "include.yaml",
        dcTargetLine = 1,
        dcTargetCol = 0,
        dcTargetEndCol = 0,
        dcDescription = "jumps from an include scalar to the included file"
      },
    DefinitionCase
      { dcDirectory = "17-inherit",
        dcSourceLine = 5,
        dcSourceCol = 6,
        dcSourceEndCol = 18,
        dcTargetFile = "inherit.yaml",
        dcTargetLine = 1,
        dcTargetCol = 0,
        dcTargetEndCol = 0,
        dcDescription = "jumps from an inherit base scalar to the base file"
      }
  ]

isOverriddenField :: Maybe InheritFieldDiff -> Bool
isOverriddenField (Just FieldOverridden {}) = True
isOverriddenField _ = False

isInheritedField :: Maybe InheritFieldDiff -> Bool
isInheritedField (Just FieldInherited {}) = True
isInheritedField _ = False

hasBaseOrigin :: FilePath -> Pos -> Maybe InheritFieldDiff -> Bool
hasBaseOrigin expectedFile expectedPos (Just FieldOverridden {..}) =
  normalise ifdBaseFile == normalise expectedFile && sameCoordinates ifdBasePos expectedPos
hasBaseOrigin expectedFile expectedPos (Just FieldInherited {..}) =
  normalise ifdBaseFile == normalise expectedFile && sameCoordinates ifdBasePos expectedPos
hasBaseOrigin _ _ _ = False

sameCoordinates :: Pos -> Pos -> Bool
sameCoordinates left right = left.posLine == right.posLine && left.posColumn == right.posColumn
