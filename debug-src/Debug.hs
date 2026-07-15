module Main where
import Yaml.Sweep
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = do
  -- Test 1: linked (should be 1 result, list1 with 2 elements = 1+1 from element-level)
  withSystemTempDirectory "config-test" $ \dir -> do
    let path = dir </> "config.yaml"
    writeFile path $ T.unpack $ T.unlines
      [ "list1:",
        "  - list2:",
        "      - key21: !zip_a [1, 2]",
        "    key1: !zip_a [1, 2]"
      ]
    result <- loadConfigValue path
    case result of
      Right results -> do
        putStrLn $ "Test 1 (linked): " <> show (length results) <> " results"
        mapM_ (\(SweepResult v) -> case v of
          Object o -> case KM.lookup "list1" o of
            Just (Array a) -> putStrLn $ "  list1 has " <> show (length a) <> " elements"
            _ -> putStrLn "  no list1"
          _ -> putStrLn "  not object") results
      Left err -> putStrLn $ "  Error: " <> err

  -- Test 2: not linked (should be 1 result, list1 with 2+2=4 elements)
  withSystemTempDirectory "config-test" $ \dir -> do
    let path = dir </> "config.yaml"
    writeFile path $ T.unpack $ T.unlines
      [ "list1:",
        "  - list2:",
        "      - key21: !zip_b [1, 2]",
        "    key1: !zip_a [1, 2]"
      ]
    result <- loadConfigValue path
    case result of
      Right results -> do
        putStrLn $ "Test 2 (not linked): " <> show (length results) <> " results"
        mapM_ (\(SweepResult v) -> case v of
          Object o -> case KM.lookup "list1" o of
            Just (Array a) -> putStrLn $ "  list1 has " <> show (length a) <> " elements"
            _ -> putStrLn "  no list1"
          _ -> putStrLn "  not object") results
      Left err -> putStrLn $ "  Error: " <> err
