module Main (main) where

import LspSpec qualified
import Runner (runTestCases)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let examplesDir = cwd </> "examples"
  hspec do
    runTestCases examplesDir
    LspSpec.spec examplesDir
