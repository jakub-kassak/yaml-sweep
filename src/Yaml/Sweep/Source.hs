module Yaml.Sweep.Source
  ( SourceReadError (..),
    SourceReader,
    readYamlFile,
    overlaySourceReader,
  )
where

import Data.Text.Encoding qualified as TE
import System.Directory (doesFileExist)

data SourceReadError = SourceNotFound | SourceUtf8Error Text

type SourceReader = FilePath -> IO (Either SourceReadError Text)

readYamlFile :: SourceReader
readYamlFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left SourceNotFound)
    else do
      bytes <- readFileBS path
      pure $ first (SourceUtf8Error . toText . displayException) (TE.decodeUtf8' bytes)

overlaySourceReader :: (FilePath -> IO (Maybe Text)) -> SourceReader -> SourceReader
overlaySourceReader readOverlay fallback path =
  readOverlay path >>= maybe (fallback path) (pure . Right)
