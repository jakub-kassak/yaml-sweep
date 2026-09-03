module Main (main) where

import Data.Aeson (Value (..), encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Scientific (fromFloatDigits)
import Data.Vector qualified as Vector
import Data.YAML (Node (..), Pos, Scalar (..))
import Yaml.Sweep (loadConfigValue)
import Yaml.Sweep.Types (renderErr)

main :: IO ()
main = do
  args <- getArgs
  path <- case args of
    [value] -> pure value
    _ -> die "Usage: yaml-sweep-json <path-to-yaml-file>"
  loadConfigValue path >>= either (die . renderErr) (either die (BL8.putStrLn . encode) . traverse nodeToJson)

nodeToJson :: Node Pos -> Either String Value
nodeToJson = \case
  Scalar _ scalar -> Right (scalarToJson scalar)
  Mapping _ _ fields -> Object . KM.fromList <$> traverse mappingToJson (Map.toList fields)
  Sequence _ _ values -> Array . Vector.fromList <$> traverse nodeToJson values
  Anchor _ _ node -> nodeToJson node

mappingToJson :: (Node Pos, Node Pos) -> Either String (Key.Key, Value)
mappingToJson (key, value) = do
  name <- nodeKeyToText key
  json <- nodeToJson value
  pure (Key.fromText name, json)

nodeKeyToText :: Node Pos -> Either String Text
nodeKeyToText = \case
  Scalar _ (SStr value) -> Right value
  Scalar _ (SUnknown _ value) -> Right value
  _ -> Left "YAML mapping keys must be strings to emit JSON"

scalarToJson :: Scalar -> Value
scalarToJson = \case
  SNull -> Null
  SBool value -> Bool value
  SFloat value -> Number (fromFloatDigits value)
  SInt value -> Number (fromInteger value)
  SStr value -> String value
  SUnknown _ value -> String value
