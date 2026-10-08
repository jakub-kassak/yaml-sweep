-- | Pass 1b: Convert HsYAML Node tree into a ConfigExpr tree.
module Yaml.Sweep.Parser
  ( nodeToExpr,
    deepMerge,
  )
where

import Control.Monad (foldM)
import Data.Map.Strict qualified as M
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.YAML (Mapping, Node (..), Pos (..), Scalar (..))
import Data.YAML.Event (mkTag, tagToText)
import System.FilePath (takeDirectory)
import Yaml.Sweep.Loader
  ( DocSnapshot (..),
    YamlCache,
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
nodeToExpr cache currentFile node = CEOrigin currentFile <$> nodeToExprAtRoot True cache currentFile node

nodeToExprAtRoot :: Bool -> YamlCache -> FilePath -> Node Pos -> Either Err ConfigExpr
nodeToExprAtRoot isDocumentRoot cache currentFile node = case node of
  Scalar pos (SUnknown tag text)
    | tagToText tag == Just "!include" -> do
        let baseDir = takeDirectory currentFile
            path = resolvePath baseDir (T.unpack text)
        cachedSnapshot <- cache M.!? path & note ([i|!include file not in cache: #{path}|], pos, currentFile)
        inner <- nodeToExpr cache path cachedSnapshot.dsNode
        return $ CEFileBarrier pos path inner
    | Just name <- tagToText tag >>= T.stripPrefix "!var_" -> do
        when (T.null name) $
          Left ([i|!var_ with empty name|], pos, currentFile)
        return $ CEVarRef name pos
  Scalar {} -> return $ CEScalar node
  Mapping pos tag mapping
    | tagToText tag == Just "!inherit" -> do
        merged <- resolveInherit isDocumentRoot cache currentFile pos mapping
        return merged
  Mapping _ _ mapping -> do
    when isDocumentRoot $
      ensureDistinctDeclarations currentFile mapping
    fields <- traverse (processMappingKey isDocumentRoot cache currentFile) (M.toList mapping)
    return $ CEObject (nodeAnn node) (M.fromList fields)
  Sequence pos tag nodes
    | tagToText tag == Just "!prod" -> do
        when (null nodes) $
          Left ([i|!prod with empty list|], pos, currentFile)
        let zipNode = Sequence pos (mkTag $ "!zip_" <> T.unpack (freshKey currentFile pos)) nodes
        nodeToExprAtRoot isDocumentRoot cache currentFile zipNode
  Sequence pos tag nodes
    | Just zipName <- tagToText tag >>= T.stripPrefix "!zip_" -> do
        when (T.null zipName) $
          Left ([i|!zip_ with empty scope name|], pos, currentFile)
        when (null nodes) $
          Left ([i|!zip_#{zipName} with empty list|], pos, currentFile)
        traverse (nodeToExprAtRoot False cache currentFile) nodes
          <&> CEZip pos zipName
  Sequence _ _ nodes ->
    CEArray (nodeAnn node) <$> traverse (nodeToExprAtRoot False cache currentFile) nodes
  Anchor _ _ inner -> nodeToExprAtRoot isDocumentRoot cache currentFile inner

processMappingKey :: Bool -> YamlCache -> FilePath -> (Node Pos, Node Pos) -> Either Err (Node (), (Pos, ConfigExpr))
processMappingKey isDocumentRoot cache currentFile (kNode, vNode) =
  case keyTag kNode of
    Just (KeyDeclaration name) -> do
      when (T.null name) $
        Left ([i|!key with empty name|], nodeAnn kNode, currentFile)
      return (plainKeyNode name, (nodeAnn kNode, CEKeyDecl name (nodeAnn kNode)))
    Just (GlobalKeyDeclaration name)
      | not isDocumentRoot -> Left ([i|!global_key declarations are only allowed at a document root|], nodeAnn kNode, currentFile)
      | T.null name -> Left ([i|!global_key with empty name|], nodeAnn kNode, currentFile)
      | otherwise -> return (plainKeyNode name, (nodeAnn kNode, CEGlobalKeyDecl name (nodeAnn kNode)))
    Just (VariableDeclaration isGlobal name)
      | not isDocumentRoot -> Left ([i|!#{variableDeclarationName isGlobal} declarations are only allowed at a document root|], nodeAnn kNode, currentFile)
      | T.null name -> Left ([i|!#{variableDeclarationName isGlobal} with empty name|], nodeAnn kNode, currentFile)
      | otherwise -> do
          val <- nodeToExprAtRoot False cache currentFile vNode
          return (void kNode, (nodeAnn kNode, CEVarDecl isGlobal name (nodeAnn kNode) val))
    Nothing -> do
      val <- nodeToExprAtRoot False cache currentFile vNode
      return (void kNode, (nodeAnn kNode, val))
  where
    plainKeyNode name = Scalar () (SStr name)

    keyTag (Scalar _ (SUnknown tag text))
      | tagToText tag == Just "!key" = Just (KeyDeclaration text)
      | tagToText tag == Just "!global_key" = Just (GlobalKeyDeclaration text)
      | tagToText tag == Just "!set" = Just (VariableDeclaration False text)
      | tagToText tag == Just "!global_set" = Just (VariableDeclaration True text)
    keyTag _ = Nothing

data KeyDeclaration
  = KeyDeclaration Text
  | GlobalKeyDeclaration Text
  | VariableDeclaration Bool Text

ensureDistinctDeclarations :: FilePath -> Mapping Pos -> Either Err ()
ensureDistinctDeclarations currentFile = void . foldM addDeclaration M.empty . M.toList
  where
    addDeclaration declarations (key, _) = case keyTag key of
      Just (VariableDeclaration isGlobal name)
        | Just previousPos <- declarations M.!? (isGlobal, name) ->
            Left ([i|duplicate variable declaration: '#{name}'|], previousPos, currentFile)
        | otherwise -> Right (M.insert (isGlobal, name) (nodeAnn key) declarations)
      _ -> Right declarations

    keyTag (Scalar _ (SUnknown tag text))
      | tagToText tag == Just "!set" = Just (VariableDeclaration False text)
      | tagToText tag == Just "!global_set" = Just (VariableDeclaration True text)
    keyTag _ = Nothing

variableDeclarationName :: Bool -> Text
variableDeclarationName isGlobal = if isGlobal then "global_set" else "set"

freshKey :: FilePath -> Pos -> Text
freshKey file pos =
  (file ++ show (posLine pos) ++ show (posColumn pos))
    & foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int)
    & abs
    & show
    & T.pack
    & ("__prod_" <>)

resolveInherit :: Bool -> YamlCache -> FilePath -> Pos -> Mapping Pos -> Either Err ConfigExpr
resolveInherit isDocumentRoot cache currentFile pos mapping = do
  baseText <-
    note
      ([i|!inherit requires a 'base' key|], pos, currentFile)
      (lookupScalar mapping "base")
  let baseDir = takeDirectory currentFile
      basePath = resolvePath baseDir (T.unpack baseText)
  baseSnapshot <-
    note
      ([i|!inherit base file not in cache: #{basePath}|], pos, currentFile)
      (M.lookup basePath cache)
  baseExpr <- nodeToExpr cache basePath baseSnapshot.dsNode
  let overrideMapping = M.filterWithKey (\k _ -> scalarText k /= Just "base") mapping
  when isDocumentRoot $
    ensureDistinctDeclarations currentFile overrideMapping
  overrideFields <- traverse (processMappingKey isDocumentRoot cache currentFile) (M.toList overrideMapping)
  return (CEInherit pos basePath baseExpr (CEObject pos (M.fromList overrideFields)))

-- | Deep-merge two 'ConfigExpr' trees.  On mapping-key collision the
-- position of the *second* (override) side wins, while the values are
-- merged recursively.  Keys are positionless 'Node ()' so 'Map'
-- equality is returnly structural.
deepMerge :: ConfigExpr -> ConfigExpr -> ConfigExpr
deepMerge (CEOrigin _ inner) override = deepMerge inner override
deepMerge (CEFileBarrier _ _ inner) override = deepMerge inner override
deepMerge (CEInherit _ _ base inner) override = deepMerge (deepMerge base inner) override
deepMerge (CEObject _ base) (CEObject overridePos override) = CEObject overridePos (mergeObj base override)
  where
    mergeObj = M.unionWith \(_, v1) (pos2, v2) -> (pos2, deepMerge v1 v2)
deepMerge _ override = override
