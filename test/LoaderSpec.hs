module LoaderSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Yaml.Sweep (ConfigLoadResult (..), loadConfigValueWithSnapshot)
import Yaml.Sweep.Loader (DocSnapshot (..), YamlCache, loadReferencedYamlDocumentsWith)
import Yaml.Sweep.Source (overlaySourceReader, readYamlFile)
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
            sourceAt rootPath clrCache `shouldBe` Just rootSource
            sourceAt childPath clrCache `shouldBe` Just childSource
            sourceAt basePath clrCache `shouldBe` Just baseSource
            sourceAt leafPath clrCache `shouldBe` Just leafSource

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
            Map.keys clrCache `shouldMatchList` [canonicalRootPath, childPath, basePath]
            sourceAt basePath clrCache `shouldBe` Just baseSource

    it "uses disk sources outside an editor overlay" $
      withSystemTempDirectory "yaml-sweep-source-reader" $ \directory -> do
        let rootPath = directory </> "root.yaml"
            childPath = directory </> "child.yaml"
            rootSource = "child: !include child.yaml\n"
            overlaySource = "child: !include child.yaml\nsource: editor\n"
            childSource = "source: disk\n"
            readOverlay path
              | path == rootPath = pure (Just overlaySource)
              | otherwise = pure Nothing
        TIO.writeFile rootPath rootSource
        TIO.writeFile childPath childSource
        result <- loadReferencedYamlDocumentsWith (overlaySourceReader readOverlay readYamlFile) rootPath
        case result of
          Left err -> expectationFailure (renderErr err)
          Right cache -> do
            sourceInCache rootPath cache `shouldBe` Just overlaySource
            sourceInCache childPath cache `shouldBe` Just childSource

sourceAt :: FilePath -> YamlCache -> Maybe Text
sourceAt path cache = dsSource <$> Map.lookup path cache

sourceInCache :: FilePath -> YamlCache -> Maybe Text
sourceInCache path cache = dsSource <$> Map.lookup path cache
