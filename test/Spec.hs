module Main (main) where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Runner (runTestCases)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let examplesDir = cwd </> "examples"
  hspec (runTestCases examplesDir)
