{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pass 2: Scope resolution and sweep expansion.
module Yaml.Sweep.Expander
  ( resolveAndExpand,
    exprToValue,
  )
where

import Control.Monad (foldM)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.IntMap.Strict qualified as IM
import Data.List (foldl1')
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.YAML (Pos (..))
import Yaml.Sweep.Types

------------------------------------------------------------------------------
-- Top-level entry point
------------------------------------------------------------------------------

resolveAndExpand :: ConfigExpr -> Either String [SweepResult]
resolveAndExpand root =
  expand 0 M.empty (CEArray [root]) <&> \case
    Just (Single (SVArray im)) -> IM.elems im & map (SweepResult . sweepToValue)
    _ -> error "unreachable"

------------------------------------------------------------------------------
-- Helper data structures
------------------------------------------------------------------------------

data SweepValue
  = SVScalar Value
  | SVObject (KM.KeyMap SweepValue)
  | SVArray (IM.IntMap SweepValue)
  deriving (Show, Eq)

data SweepVariant a = Single a | Expand (M.Map T.Text [a])
  deriving (Show)

instance Functor SweepVariant where
  fmap f (Single v) = Single (f v)
  fmap f (Expand m) = Expand (fmap (fmap f) m)

------------------------------------------------------------------------------
-- Deep merge on SweepValue
------------------------------------------------------------------------------

mergeDeep :: SweepValue -> SweepValue -> SweepValue
mergeDeep (SVObject o1) (SVObject o2) = SVObject (KM.unionWith mergeDeep o1 o2)
mergeDeep (SVArray a1) (SVArray a2) = SVArray (IM.unionWith mergeDeep a1 a2)
mergeDeep _ v2 = v2

------------------------------------------------------------------------------
-- Pass 1: preScan
------------------------------------------------------------------------------

preScan :: ScopeEnv -> Int -> ConfigExpr -> Either String ScopeEnv
preScan env depth = \case
  CEScalar _ -> Right env
  CEFileBarrier _ -> Right env
  CEArray _ -> Right env
  CEKeyDecl name pos _ ->
    case M.lookup name env of
      Just entry
        | seScopeIdx entry < depth ->
            Left ("shadowing: " <> T.unpack name <> " was declared in an outer scope")
        | seIsExplicit entry ->
            Left ("double definition: " <> T.unpack name <> " was already declared at the same scope")
      _ -> Right (M.insert name (ScopeEntry True pos depth) env)
  CEObject fields -> foldlM ((. snd) . preScanAcc) env fields
  CEZip _ pos name exprs ->
    (env M.!? name)
      & maybe (M.insert name (ScopeEntry False pos depth) env) (const env)
      & flip (foldlM preScanAcc) exprs
  CEProd fp pos exprs ->
    M.insert (freshKey fp pos) (ScopeEntry False pos depth) env
      & flip (foldlM preScanAcc) exprs
  where
    preScanAcc = preScan ?? depth

------------------------------------------------------------------------------
-- Pass 2: expand
------------------------------------------------------------------------------

expand :: Int -> ScopeEnv -> ConfigExpr -> Either String (Maybe (SweepVariant SweepValue))
expand depth env = \case
  CEScalar v -> v & SVScalar & Single & Just & Right
  CEKeyDecl {} -> Right Nothing
  CEObject keyedFields ->
    traverse (traverse (expand depth env)) keyedFields
      <&> (\fs -> [fmap (singletonObj k) v | (k, Just v) <- fs])
      >>= foldM merge (Single $ SVObject KM.empty)
        <&> Just
  CEArray items ->
    mapM expandElem items
      <&> catMaybes
      <&> concatMap (expandLocalScopes env)
      <&> zip [0 ..]
      <&> map (\(i, v) -> fmap (SVArray . IM.singleton i) v)
      >>= foldM merge (Single $ SVArray IM.empty)
        <&> Just
  CEZip _ _ key items ->
    mapM (expand depth env) items
      <&> catMaybes
      >>= traverse mergeSingle
        <&> M.singleton key
        <&> Expand
        <&> Just
  CEProd fp pos items ->
    expand depth env (CEZip fp pos (freshKey fp pos) items)
  CEFileBarrier inner ->
    expand 0 M.empty inner >>= traverse (fmap Single . mergeSingle)
  where
    expandElem e = preScan env (depth + 1) e >>= (expand (depth + 1) ?? e)

    mergeSingle (Single v) = Right v
    mergeSingle (Expand _) = Left "Zip over zip boundary is not supported"

    singletonObj k v = SVObject (KM.singleton (Key.fromText k) v)

zipEqual :: [a] -> [b] -> Either String [(a, b)]
zipEqual xs ys
  | length xs == length ys = Right (zip xs ys)
  | otherwise = Left "inconsistent lengths"

expandLocalScopes :: ScopeEnv -> SweepVariant SweepValue -> [SweepVariant SweepValue]
expandLocalScopes _ s@(Single _) = [s]
expandLocalScopes env (Expand m)
  | M.null notInEnv = [Expand m]
  | M.null inEnv = mergedLocals & map Single
  | otherwise = mergedLocals & map (\loc -> fmap (`mergeDeep` loc) (Expand inEnv))
  where
    (inEnv, notInEnv) = M.partitionWithKey (\k _ -> M.member k env) m
    mergedLocals = M.elems notInEnv & sequence & map (foldl1' mergeDeep)

merge :: SweepVariant SweepValue -> SweepVariant SweepValue -> Either String (SweepVariant SweepValue)
merge (Single a1) (Single a2) = a1 `mergeDeep` a2 & Single & Right
merge (Single a) e@(Expand _) = fmap (a `mergeDeep`) e & Right
merge e@(Expand _) (Single a) = fmap (`mergeDeep` a) e & Right
merge (Expand e1) (Expand e2) = foldM combine e1 (M.toList e2) <&> Expand
  where
    combine accMap (k, ys) = M.alterF (update ys) k accMap
    update ys Nothing = Right (Just ys)
    update ys (Just xs) = zipEqual xs ys <&> map (uncurry mergeDeep) <&> Just

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

sweepToValue :: SweepValue -> Value
sweepToValue (SVScalar v) = v
sweepToValue (SVObject o) = Object (fmap sweepToValue o)
sweepToValue (SVArray a) = IM.elems a & map sweepToValue & V.fromList & Array

exprToValue :: ConfigExpr -> Either String Value
exprToValue _ = Left "exprToValue should use resolveAndExpand"
