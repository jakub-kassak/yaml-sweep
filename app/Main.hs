module Main where

import Data.ByteString.Char8 qualified as BS
import Data.YAML (Doc (..), encodeNode)
import Yaml.Sweep (loadConfigValue)
import Yaml.Sweep.Types (renderErr)

main :: IO ()
main = do
  args <- getArgs
  path <- case args of
    [p] -> pure p
    _ -> die "Usage: yaml-sweep-cli <path-to-yaml-file>"

  results <- loadConfigValue path >>= either (die . renderErr) pure
  forM_ (zip [(1 :: Int) ..] results) $ \(i, node) -> do
    putTextLn $ "---"
    BS.putStr (toStrict (encodeNode [Doc (void node)]))
