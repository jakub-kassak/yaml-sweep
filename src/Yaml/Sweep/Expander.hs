{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pass 2: Scope resolution and sweep expansion.
module Yaml.Sweep.Expander
  ( resolveAndExpand,
  )
where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IM
import Data.List (foldl1')
import Data.Map.Strict qualified as M
import Data.String.Interpolate qualified as I
import Data.Text qualified as T
import Data.YAML (Node (..), Pos (..))
import Data.YAML.Event (Tag, mkTag)
import Yaml.Sweep.Types
  ( ConfigExpr (..),
    Err,
    ScopeEntry (..),
    ScopeEnv (..),
    emptyScopeEnv,
    incScopeDepth,
    scopeInsert,
    scopeLookup,
    scopeMember,
  )

------------------------------------------------------------------------------
-- Tags not re-exported by the public "Data.YAML" module
------------------------------------------------------------------------------

yamlTagMap :: Tag
yamlTagMap = mkTag "tag:yaml.org,2002:map"

yamlTagSeq :: Tag
yamlTagSeq = mkTag "tag:yaml.org,2002:seq"

noPos :: Pos
noPos = Pos {posByteOffset = 0, posCharOffset = 0, posLine = 1, posColumn = 0}

------------------------------------------------------------------------------
-- Top-level entry point
------------------------------------------------------------------------------

resolveAndExpand :: ConfigExpr -> Either Err [Node Pos]
resolveAndExpand root =
  expand (emptyScopeEnv "") (CEArray noPos [root])
    <&> \case
      Just (Single (SVArray _ im)) -> Right (IM.elems im & map sweepToNode)
      _ -> Left ([I.i|sweep expansion produced no top-level array|], noPos, "")
    & join

------------------------------------------------------------------------------
-- Helper data structures
------------------------------------------------------------------------------

data SweepValue
  = SVScalar !(Node Pos)
  | SVObject !Pos !(M.Map (Node ()) (Pos, SweepValue))
  | SVArray !Pos !(IM.IntMap SweepValue)
  deriving (Show, Eq)

data SweepVariant a = Single a | Expand (M.Map Text [a])
  deriving (Show)

instance Functor SweepVariant where
  fmap f (Single v) = Single (f v)
  fmap f (Expand m) = Expand (fmap (fmap f) m)

------------------------------------------------------------------------------
-- Deep merge on SweepValue
------------------------------------------------------------------------------

mergeDeep :: SweepValue -> SweepValue -> SweepValue
mergeDeep (SVObject p1 o1) (SVObject _ o2) = SVObject p1 (M.unionWith (fmap . mergeDeep . snd) o1 o2)
mergeDeep (SVArray p1 a1) (SVArray _ a2) = SVArray p1 (IM.unionWith mergeDeep a1 a2)
mergeDeep _ v2 = v2

------------------------------------------------------------------------------
-- Pass 1: preScan
------------------------------------------------------------------------------

preScan :: ScopeEnv -> ConfigExpr -> Either Err ScopeEnv
preScan env = \case
  CEScalar _ -> Right env
  CEFileBarrier _ _ _ -> Right env
  CEArray _ _ -> Right env
  CEKeyDecl name pos ->
    case scopeLookup name env of
      Just entry
        | seDepth entry < depth env ->
            Left ([I.i|shadowing: '#{name}' was declared in an outer scope|], sePos entry, seFile entry)
        | seIsExplicit entry ->
            Left ([I.i|double definition: '#{name}' was already declared at the same scope|], sePos entry, seFile entry)
      _ -> Right (scopeInsert name (ScopeEntry True pos (path env) (depth env)) env)
  CEObject _ fields -> foldlM preScan env (snd <$> M.elems fields)
  CEZip pos name exprs ->
    scopeInsertIfMissing name (ScopeEntry False pos (path env) (depth env)) env
      & flip (foldlM preScan) exprs
  CEProd _ exprs ->
    foldlM preScan env exprs
  where
    scopeInsertIfMissing name entry e =
      maybe (scopeInsert name entry e) (const e) (scopeLookup name e)

------------------------------------------------------------------------------
-- Pass 2: expand
------------------------------------------------------------------------------

expand :: ScopeEnv -> ConfigExpr -> Either Err (Maybe (SweepVariant SweepValue))
expand env = \case
  CEScalar node -> node & SVScalar & Single & Just & Right
  CEKeyDecl {} -> Right Nothing
  CEObject pos keyedFields ->
    traverse (traverse (expand env)) keyedFields
      <&> M.mapMaybeWithKey (\k (p, mv) -> fmap (singletonObj pos k p) <$> mv)
      <&> M.elems
      >>= foldM (merge pos) (Single $ SVObject pos M.empty)
        <&> Just
  CEArray pos items ->
    mapM expandElem items
      <&> catMaybes
      <&> concatMap (expandLocalScopes env)
      <&> zip [0 ..]
      <&> map (\(i, v) -> fmap (SVArray pos . IM.singleton i) v)
      >>= foldM (merge pos) (Single (SVArray pos IM.empty))
        <&> Just
  CEZip pos key items ->
    mapM (expand env) items
      <&> catMaybes
      >>= traverse (mergeSingle (zipErrMsg, pos, path env))
        <&> (Just . Expand . M.singleton key)
  CEProd pos items ->
    expand env (CEZip pos (freshKey (path env) pos) items)
  CEFileBarrier pos fp inner ->
    expand (emptyScopeEnv fp) inner >>= traverse (fmap Single . mergeSingle (fbErrMsg, pos, fp))
  where
    env' = incScopeDepth env
    expandElem e = preScan env' e >>= flip expand e

    mergeSingle _ (Single v) = Right v
    mergeSingle err (Expand _) = Left err
    fbErrMsg = "Top level keys in included file is not allowed"
    zipErrMsg = "Zip over zip boundary is not supported"

    singletonObj pos k p v = SVObject pos (M.singleton k (p, v))

merge :: Pos -> SweepVariant SweepValue -> SweepVariant SweepValue -> Either Err (SweepVariant SweepValue)
merge _ (Single a1) (Single a2) = a1 `mergeDeep` a2 & Single & Right
merge _ (Single a) e@(Expand _) = fmap (a `mergeDeep`) e & Right
merge _ e@(Expand _) (Single a) = fmap (`mergeDeep` a) e & Right
merge pos (Expand e1) (Expand e2) = foldM combine e1 (M.toList e2) <&> Expand
  where
    combine accMap (k, ys) = M.alterF (update k ys) k accMap

    update _ ys Nothing = Right (Just ys)
    update k ys (Just xs)
      | length xs == length ys = Right (Just (zipWith mergeDeep xs ys))
      | otherwise = Left ([I.i|inconsistent lengths for scope '#{k}'|], pos, "")

expandLocalScopes :: ScopeEnv -> SweepVariant SweepValue -> [SweepVariant SweepValue]
expandLocalScopes _ s@(Single _) = [s]
expandLocalScopes env (Expand m)
  | M.null notInEnv = [Expand m]
  | M.null inEnv = mergedLocals & map Single
  | otherwise = mergedLocals & map (\sv -> fmap (`mergeDeep` sv) (Expand inEnv))
  where
    (inEnv, notInEnv) = M.partitionWithKey (\k _ -> scopeMember k env) m
    mergedLocals = M.elems notInEnv & sequence & map (foldl1' mergeDeep)

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

freshKey :: FilePath -> Pos -> Text
freshKey file pos =
  (file ++ show (posLine pos) ++ show (posColumn pos))
    & foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int)
    & abs
    & show
    & T.pack
    & ("__prod_" <>)

sweepToNode :: SweepValue -> Node Pos
sweepToNode (SVScalar s) = s
sweepToNode (SVObject pos keyValues) =
  Mapping
    pos
    yamlTagMap
    (M.fromList [(fmap (const p) k, sweepToNode v) | (k, (p, v)) <- M.toList keyValues])
sweepToNode (SVArray pos im) =
  Sequence pos yamlTagSeq (map sweepToNode (IM.elems im))
