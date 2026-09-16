module LoaderSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Text.IO qualified as TIO
import Data.YAML (Mapping, Node (..), Pos, Scalar (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Yaml.Sweep (ConfigLoadResult (..), SourceSnapshot (..), loadConfigValueWithSnapshot)
import Yaml.Sweep.Loader (DocSnapshot (..), scalarText)
import Yaml.Sweep.Types (Err, renderErr)

spec :: Spec
spec =
  describe "loadConfigValueWithSnapshot" do
    it "captures exact nested include and inherit sources with expanded nodes" $
      withSystemTempDirectory "yaml-sweep-snapshot" $ \directory -> do
        let rootPath = directory </> "root.yaml"
            childPath = directory </> "child.yaml"
            basePath = directory </> "base.yaml"
            leafPath = directory </> "leaf.yaml"
            rootSource = "# root comment\npayload: !include child.yaml # include tag\n"
            childSource = "# child comment\npayload: !inherit\n  base: base.yaml # inherit tag\n  extra: !include leaf.yaml # nested include\n"
            baseSource = "# base comment\nname: !tagged base\n"
            leafSource = "# leaf comment\nvalue: !literal preserved\n"
        TIO.writeFile rootPath rootSource
        TIO.writeFile childPath childSource
        TIO.writeFile basePath baseSource
        TIO.writeFile leafPath leafSource
        result <- loadConfigValueWithSnapshot rootPath
        case result of
          Left err -> expectationFailure (renderErr err)
          Right ConfigLoadResult {..} -> do
            length clrExpandedNodes `shouldBe` 1
            sourceAt rootPath clrSourceSnapshot `shouldBe` Just rootSource
            sourceAt childPath clrSourceSnapshot `shouldBe` Just childSource
            sourceAt basePath clrSourceSnapshot `shouldBe` Just baseSource
            sourceAt leafPath clrSourceSnapshot `shouldBe` Just leafSource

    it "canonicalizes root and relative include paths in source snapshots" $
      withSystemTempDirectory "yaml-sweep-canonical-paths" $ \directory -> do
        let rootPath = directory </> "defenders" </> ".." </> "root.yaml"
            canonicalRootPath = directory </> "root.yaml"
            childPath = directory </> "defenders" </> "child.yaml"
            basePath = directory </> "base.yaml"
            rootSource = "direct: !include base.yaml\nchild: !include defenders/child.yaml\n"
            childSource = "same: !include ../base.yaml\n"
            baseSource = "name: base\n"
        TIO.writeFile canonicalRootPath rootSource
        createDirectoryIfMissing True (directory </> "defenders")
        TIO.writeFile childPath childSource
        TIO.writeFile basePath baseSource
        result <- loadConfigValueWithSnapshot rootPath
        case result of
          Left err -> expectationFailure (renderErr err)
          Right ConfigLoadResult {..} -> do
            Map.keys clrSourceSnapshot.ssDocuments `shouldMatchList` [canonicalRootPath, childPath, basePath]
            sourceAt basePath clrSourceSnapshot `shouldBe` Just baseSource

    it "loads UTF-8 text relative to the referencing YAML and expands it as a string" $
      withSystemTempDirectory "yaml-sweep-load-str" $ \directory -> do
        let rootPath = directory </> "root.yaml"
            childPath = directory </> "nested" </> "child.yaml"
            textPath = directory </> "nested" </> "prompt.txt"
            textSource = "Hello, 世界\nnot: [yaml\n"
        createDirectoryIfMissing True (directory </> "nested")
        TIO.writeFile rootPath "child: !include nested/child.yaml\n"
        TIO.writeFile childPath "prompt: !load_str prompt.txt\n"
        TIO.writeFile textPath textSource
        result <- loadConfigValueWithSnapshot rootPath
        case result of
          Left err -> expectationFailure (renderErr err)
          Right ConfigLoadResult {..} -> do
            Map.member textPath clrSourceSnapshot.ssDocuments `shouldBe` False
            Map.lookup textPath clrSourceSnapshot.ssRawTexts `shouldBe` Just textSource
            loadedString clrExpandedNodes `shouldBe` Just textSource

    it "reports a missing load_str file at its reference" $
      withSystemTempDirectory "yaml-sweep-load-str-missing" $ \directory -> do
        let rootPath = directory </> "root.yaml"
        TIO.writeFile rootPath "prompt: !load_str missing.txt\n"
        loadConfigValueWithSnapshot rootPath >>= assertErrorContains "file not found: "

    it "reports invalid UTF-8 in a load_str target" $
      withSystemTempDirectory "yaml-sweep-load-str-utf8" $ \directory -> do
        let rootPath = directory </> "root.yaml"
            textPath = directory </> "prompt.txt"
        TIO.writeFile rootPath "prompt: !load_str prompt.txt\n"
        BS.writeFile textPath (BS.pack [0xff])
        loadConfigValueWithSnapshot rootPath >>= assertErrorContains "UTF-8 decode error"

sourceAt :: FilePath -> SourceSnapshot -> Maybe Text
sourceAt path SourceSnapshot {..} = dsSource <$> Map.lookup path ssDocuments

loadedString :: [Node Pos] -> Maybe Text
loadedString [Mapping _ _ mapping] = do
  Mapping _ _ child <- lookupNode "child" mapping
  Scalar _ (SStr text) <- lookupNode "prompt" child
  pure text
loadedString _ = Nothing

lookupNode :: Text -> Mapping Pos -> Maybe (Node Pos)
lookupNode name mapping = listToMaybe [value | (key, value) <- Map.toList mapping, scalarText key == Just name]

assertErrorContains :: String -> Either Err ConfigLoadResult -> Expectation
assertErrorContains expected = either (\err -> renderErr err `shouldSatisfy` isInfixOf expected) (const (expectationFailure "expected an error"))
