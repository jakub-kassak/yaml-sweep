module LoaderSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Yaml.Sweep (ConfigLoadResult (..), SourceSnapshot (..), loadConfigValueWithSnapshot)
import Yaml.Sweep.Loader (DocSnapshot (..))
import Yaml.Sweep.Types (renderErr)

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

sourceAt :: FilePath -> SourceSnapshot -> Maybe Text
sourceAt path SourceSnapshot {..} = dsSource <$> Map.lookup path ssDocuments
