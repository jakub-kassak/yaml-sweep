-- | YAML configuration engine with scoped sweep expansion.
--
-- See README.md for full documentation.  Key concepts:
--
--   * @!key \<name\>@ declares a scope visible in the entire subtree.
--   * @!zip_\<name\>@ without a matching @!key@ opens an implicit local scope.
--   * All @!zip_\<name\>@ in the same scope are index-synchronous.
--   * Expansion bubbles to the nearest enclosing array of the declaring
--     mapping (element-level) or is config-level if not inside an array.
--   * @!prod@ is sugar for a fresh unique scope — always one occurrence.
--   * @!include@ and @!inherit@ both create file boundaries.
module Yaml.Sweep
  ( loadConfigValue,
  )
where

import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.YAML (Node, Pos)

import Yaml.Sweep.Expander (resolveAndExpand)
import Yaml.Sweep.Loader (loadAndCache)
import Yaml.Sweep.Parser (nodeToExpr)
import Yaml.Sweep.Types (Err, noPos, note)

-- | Load a YAML config file, resolve !include/!inherit, expand !prod/!zip.
--   Returns one 'Node Pos' per resulting configuration variant.  Mapping
--   keys keep their source positions; see README.md for details.
loadConfigValue :: FilePath -> IO (Either Err [Node Pos])
loadConfigValue yamlFile = runExceptT do
  cache <- ExceptT $ loadAndCache yamlFile
  rootNode <- ExceptT $ pure $ note ([i|Main file not in cache: #{yamlFile}|], noPos, yamlFile) (Map.lookup yamlFile cache)
  scoped <- ExceptT $ pure $ nodeToExpr cache yamlFile rootNode
  ExceptT . pure $ resolveAndExpand scoped
