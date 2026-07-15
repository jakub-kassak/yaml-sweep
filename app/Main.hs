module Main where

import Yaml.Sweep
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import System.FilePath (takeExtension)
import qualified Data.Yaml as Yaml

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      result <- loadConfigValue path
      case result of
        Left err -> die $ "Fehler beim Laden/Expandieren:\n" <> err
        Right results -> do
          putTextLn $ "Erfolgreich generierte Varianten (" <> show (length results) <> "):\n"
          forM_ (zip [(1::Int)..] results) $ \(i, SweepResult val) -> do
            putTextLn $ "--- Variante " <> show i <> " ---"
            BS.putStrLn (Yaml.encode val)
            putTextLn ""
    _ -> do
      die "Verwendung: yaml-sweep-cli <pfad-zur-yaml-datei>"
