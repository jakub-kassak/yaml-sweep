-- | Pass 1b: Convert HsYAML Node tree into a ConfigExpr tree.
module Yaml.Sweep.Parser
  ( nodeToExpr,
    deepMerge,
    scalarToValue,
  )
where

import Data.Aeson (Value (..))
import Data.List (lookup, nub)
import Data.Map.Strict qualified as Map
import Data.Scientific (fromFloatDigits)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.YAML (Mapping, Node (..), Pos (..), Scalar (..))
import Data.YAML.Event (tagToText)
import System.FilePath (takeDirectory)

import Yaml.Sweep.Loader
  ( YamlCache,
    lookupScalar,
    nodeAnn,
    nodeKey,
    resolvePath,
    scalarText,
  )
import Yaml.Sweep.Types

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

keyTag :: Node Pos -> Maybe Text
keyTag (Scalar _ (SUnknown tag text))
  | tagToText tag == Just "!key" = Just text
keyTag _ = Nothing

nodeToExpr :: YamlCache -> FilePath -> Node Pos -> Either String ConfigExpr
nodeToExpr cache currentFile node = case node of
  Scalar pos (SUnknown tag text)
    | tagToText tag == Just "!include" -> do
        let baseDir = takeDirectory currentFile
            path = resolvePath baseDir (T.unpack text)
        cachedNode <- note
          [i|!include file not in cache: #{path} (referenced in #{currentFile} at line #{posLine pos}, column #{posColumn pos})|]
          (Map.lookup path cache)
        inner <- nodeToExpr cache path cachedNode
        pure (CEFileBarrier inner)
  Scalar _ s -> pure (CEScalar (scalarToValue s))
  Mapping pos tag mapping
    | tagToText tag == Just "!inherit" -> do
        merged <- resolveInherit cache currentFile pos mapping
        pure (CEFileBarrier merged)
  Mapping _ _ mapping -> do
    fields <- traverse (processMappingKey cache currentFile) (Map.toList mapping)
    pure (CEObject fields)
  Sequence pos tag nodes
    | tagToText tag == Just "!prod" -> do
        choices <- traverse (nodeToExpr cache currentFile) nodes
        when (null choices) $
          Left [i|!prod with empty list in #{currentFile} at line #{posLine pos}|]
        pure (CEProd currentFile pos choices)
  Sequence pos tag nodes
    | Just zipName <- tagToText tag >>= T.stripPrefix "!zip_" -> do
        when (T.null zipName) $
          Left [i|!zip_ with empty scope name in #{currentFile} at line #{posLine pos}|]
        choices <- traverse (nodeToExpr cache currentFile) nodes
        when (null choices) $
          Left [i|!zip_#{zipName} with empty list in #{currentFile} at line #{posLine pos}|]
        pure (CEZip currentFile pos zipName choices)
  Sequence _ _ nodes ->
    CEArray <$> traverse (nodeToExpr cache currentFile) nodes
  Anchor _ _ inner -> nodeToExpr cache currentFile inner

processMappingKey :: YamlCache -> FilePath -> (Node Pos, Node Pos) -> Either String (Text, ConfigExpr)
processMappingKey cache currentFile (kNode, vNode) =
  case keyTag kNode of
    Just name -> do
      when (T.null name) $
        Left [i|!key with empty name in #{currentFile} at line #{posLine (nodeAnn kNode)}|]
      pure (name, CEKeyDecl name (nodeAnn kNode) currentFile)
    Nothing -> do
      key <- nodeKey currentFile kNode
      val <- nodeToExpr cache currentFile vNode
      pure (key, val)

resolveInherit :: YamlCache -> FilePath -> Pos -> Mapping Pos -> Either String ConfigExpr
resolveInherit cache currentFile pos mapping = do
  baseText <- note
    [i|!inherit in #{currentFile} at line #{posLine pos} requires a 'base' key|]
    (lookupScalar mapping "base")
  let baseDir = takeDirectory currentFile
      basePath = resolvePath baseDir (T.unpack baseText)
  baseNode <- note
    [i|!inherit base file not in cache: #{basePath}|]
    (Map.lookup basePath cache)
  baseExpr <- nodeToExpr cache basePath baseNode
  let overrideMapping = Map.filterWithKey (\k _ -> scalarText k /= Just "base") mapping
  overrideFields <- traverse (processMappingKey cache currentFile) (Map.toList overrideMapping)
  pure (deepMerge baseExpr (CEObject overrideFields))

deepMerge :: ConfigExpr -> ConfigExpr -> ConfigExpr
deepMerge (CEFileBarrier inner) override = deepMerge inner override
deepMerge (CEObject base) (CEObject override) = CEObject (mergeFields base override)
  where
    mergeFields bs os =
      let baseKeys = fst <$> bs
          overrideKeys = fst <$> os
          allKeys = nub (baseKeys <> overrideKeys)
       in allKeys <&> \k ->
            case (lookup k bs, lookup k os) of
              (Just bv, Just ov) -> (k, deepMerge bv ov)
              (Just bv, Nothing) -> (k, bv)
              (Nothing, Just ov) -> (k, ov)
              (Nothing, Nothing) -> (k, CEScalar Null)
deepMerge _ override = override

scalarToValue :: Scalar -> Value
scalarToValue SNull = Null
scalarToValue (SBool b) = Bool b
scalarToValue (SFloat d) = Number (fromFloatDigits d)
scalarToValue (SInt intVal) = Number (fromInteger intVal)
scalarToValue (SStr t) = String t
scalarToValue (SUnknown _ t) = String t
