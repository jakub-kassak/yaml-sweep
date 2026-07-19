module Runner (runTestCases) where

import Control.Monad.Except (liftEither)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.YAML (Doc (..), decodeNode)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import Test.Hspec
import Yaml.Sweep (loadConfigValue)
import Yaml.Sweep.Types (renderErr)

------------------------------------------------------------------------------
-- Running a single test directory
------------------------------------------------------------------------------

-- | A test lives in its own directory under @examples/@. Layout:
--
--   * @config.yaml@  — entry point passed to 'loadConfigValue'
--   * any number of supporting @.yaml@ files (used via @!include@)
--   * @result.yaml@  — multi-document YAML, one document per expected
--     variant (success case)
--   * @error.txt@    — plain text with a substring expected in the rendered
--     error (failure case)
--
-- Exactly one of @result.yaml@ / @error.txt@ must be present. Files are
-- read in place from the example directory, so relative @!include@ paths
-- resolve naturally.
runTestDir :: FilePath -> Expectation
runTestDir dir = do
  entries <- listDirectory dir
  let hasResult = "result.yaml" `elem` entries
      hasError = "error.txt" `elem` entries
  case (hasResult, hasError) of
    (True, False) -> runSuccess dir
    (False, True) -> runFailure dir
    (True, True) -> expectationFailure ("both result.yaml and error.txt in " <> dir)
    (False, False) -> expectationFailure ("no result.yaml or error.txt in " <> dir)

runSuccess :: FilePath -> Expectation
runSuccess dir =
  runExceptT do
    actuals <-
      loadConfigValue (dir </> "config.yaml")
        <&> first (\e -> "Expected success, got error: " <> renderErr e)
        & ExceptT
    bytes <-
      BL.readFile (dir </> "result.yaml")
        & liftIO
    docs <-
      decodeNode bytes
        & first (\(p, e) -> "Invalid result.yaml: " <> show e <> " at " <> show p)
        & liftEither

    shouldMatchList (map void actuals) (map (void . docRoot) docs)
      & liftIO
    >>= either expectationFailure pure

runFailure :: FilePath -> Expectation
runFailure dir = do
  expectedGrep <- T.strip <$> TIO.readFile (dir </> "error.txt")
  result <- loadConfigValue (dir </> "config.yaml")
  case result of
    Right _ -> expectationFailure "Expected error, got success"
    Left err -> renderErr err `shouldContain` T.unpack expectedGrep

------------------------------------------------------------------------------
-- Discovery
------------------------------------------------------------------------------

-- | Collect all immediate subdirectories of @dir@, sorted. Each subdirectory
-- is treated as one test case.
collectTestDirs :: FilePath -> IO [FilePath]
collectTestDirs dir = do
  isDir <- doesDirectoryExist dir
  if not isDir
    then pure []
    else do
      entries <- sort <$> listDirectory dir
      let full = map (dir </>) entries
      filterM doesDirectoryExist full

-- | Walk @dir@ for test subdirectories and register each as an hspec example.
-- The example description is the subdirectory name.
runTestCases :: FilePath -> Spec
runTestCases dir = do
  dirs <- runIO (collectTestDirs dir)
  forM_ dirs \d -> it (takeFileName d) (runTestDir d)
