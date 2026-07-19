-- | Pass 1b: Convert HsYAML Node tree into a ConfigExpr tree.
module Yaml.Sweep.Parser
  ( nodeToExpr,
    deepMerge,
  )
where

import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.YAML (Mapping, Node (..), Pos (..), Scalar (..))
import Data.YAML.Event (tagToText)
import System.FilePath (takeDirectory)
import Yaml.Sweep.Loader
  ( YamlCache,
    lookupScalar,
    nodeAnn,
    resolvePath,
    scalarText,
  )
import Yaml.Sweep.Types
  ( ConfigExpr (..),
    Err,
    note,
  )


-- | A plain @!!str@ key node carrying no position; the source
-- position is tracked separately in the 'CEObject' value.  Used to
-- drop the @!key@ decoration while keeping a structural key for
-- 'Map (Node ()) ...' lookups.
nodeToExpr :: YamlCache -> FilePath -> Node Pos -> Either Err ConfigExpr
nodeToExpr cache currentFile node = case node of
  Scalar pos (SUnknown tag text)
    | tagToText tag == Just "!include" -> do
        let baseDir = takeDirectory currentFile
            path = resolvePath baseDir (T.unpack text)
        cachedNode <-
          note
            ([i|!include file not in cache: #{path}|], pos, currentFile)
            (Map.lookup path cache)
        inner <- nodeToExpr cache path cachedNode
        pure (CEFileBarrier pos currentFile inner)
  Scalar {} -> pure (CEScalar node)
  Mapping pos tag mapping
    | tagToText tag == Just "!inherit" -> do
        merged <- resolveInherit cache currentFile pos mapping
        pure (CEFileBarrier pos currentFile merged)
  Mapping _ _ mapping -> do
    fields <- traverse (processMappingKey cache currentFile) (Map.toList mapping)
    pure (CEObject (nodeAnn node) (Map.fromList fields))
  Sequence pos tag nodes
    | tagToText tag == Just "!prod" -> do
        choices <- traverse (nodeToExpr cache currentFile) nodes
        when (null choices) $
          Left ([i|!prod with empty list|], pos, currentFile)
        pure (CEProd pos choices)
  Sequence pos tag nodes
    | Just zipName <- tagToText tag >>= T.stripPrefix "!zip_" -> do
        when (T.null zipName) $
          Left ([i|!zip_ with empty scope name|], pos, currentFile)
        choices <- traverse (nodeToExpr cache currentFile) nodes
        when (null choices) $
          Left ([i|!zip_#{zipName} with empty list|], pos, currentFile)
        pure (CEZip pos zipName choices)
  Sequence _ _ nodes ->
    CEArray (nodeAnn node) <$> traverse (nodeToExpr cache currentFile) nodes
  Anchor _ _ inner -> nodeToExpr cache currentFile inner

processMappingKey :: YamlCache -> FilePath -> (Node Pos, Node Pos) -> Either Err (Node (), (Pos, ConfigExpr))
processMappingKey cache currentFile (kNode, vNode) =
  case keyTag kNode of
    Just name -> do
      when (T.null name) $
        Left ([i|!key with empty name|], nodeAnn kNode, currentFile)
      pure (plainKeyNode name, (nodeAnn kNode, CEKeyDecl name (nodeAnn kNode)))
    Nothing -> do
      val <- nodeToExpr cache currentFile vNode
      pure (void kNode, (nodeAnn kNode, val))
  where
    plainKeyNode name = Scalar () (SStr name)

    keyTag (Scalar _ (SUnknown tag text))
      | tagToText tag == Just "!key" = Just text
    keyTag _ = Nothing

resolveInherit :: YamlCache -> FilePath -> Pos -> Mapping Pos -> Either Err ConfigExpr
resolveInherit cache currentFile pos mapping = do
  baseText <-
    note
      ([i|!inherit requires a 'base' key|], pos, currentFile)
      (lookupScalar mapping "base")
  let baseDir = takeDirectory currentFile
      basePath = resolvePath baseDir (T.unpack baseText)
  baseNode <-
    note
      ([i|!inherit base file not in cache: #{basePath}|], pos, currentFile)
      (Map.lookup basePath cache)
  baseExpr <- nodeToExpr cache basePath baseNode
  let overrideMapping = Map.filterWithKey (\k _ -> scalarText k /= Just "base") mapping
  overrideFields <- traverse (processMappingKey cache currentFile) (Map.toList overrideMapping)
  pure (deepMerge baseExpr (CEObject pos (Map.fromList overrideFields)))

-- | Deep-merge two 'ConfigExpr' trees.  On mapping-key collision the
-- position of the *second* (override) side wins, while the values are
-- merged recursively.  Keys are positionless 'Node ()' so 'Map'
-- equality is purely structural.
deepMerge :: ConfigExpr -> ConfigExpr -> ConfigExpr
deepMerge (CEFileBarrier _ _ inner) override = deepMerge inner override
deepMerge (CEObject _ base) (CEObject overridePos override) = CEObject overridePos (mergeObj base override)
  where
    mergeObj = Map.unionWith \(_, v1) (pos2, v2) -> (pos2, deepMerge v1 v2)
deepMerge _ override = override
