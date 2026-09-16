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
import Data.YAML (Node (..), Pos (..), Scalar (..))
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
  = SVScalar (Node Pos)
  | SVObject Pos (M.Map (Node ()) (Pos, SweepValue))
  | SVArray Pos (IM.IntMap SweepValue)
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
  CELoadStr {} -> Right env
  CEFileBarrier _ _ _ -> Right env
  CEInherit _ _ _ override -> preScan env override
  CEArray _ _ -> Right env
  CEKeyDecl name pos ->
    case scopeLookup name env of
      Just entry
        | seDepth entry < depth env ->
            Left ([I.i|shadowing: '#{name}' was declared in an outer scope|], sePos entry, seFile entry)
        | seIsExplicit entry ->
            Left ([I.i|double definition: '#{name}' was already declared at the same scope|], sePos entry, seFile entry)
      _ -> Right (scopeInsert name (newScopeEntry True pos env) env)
  CEObject _ fields -> foldlM preScan env (snd <$> M.elems fields)
  CEZip pos name exprs -> do
    traverse_ (preScan (emptyScopeEnv (path env))) exprs
    pure (scopeInsertIfMissing name (newScopeEntry False pos env) env)
  CEProd pos exprs ->
    preScan env (CEZip pos (freshKey (path env) pos) exprs)
  where
    scopeInsertIfMissing name entry e =
      maybe (scopeInsert name entry e) (const e) (scopeLookup name e)

newScopeEntry :: Bool -> Pos -> ScopeEnv -> ScopeEntry
newScopeEntry seIsExplicit sePos ScopeEnv {..} = ScopeEntry {..}
  where
    seFile = path
    seDepth = depth

------------------------------------------------------------------------------
-- Pass 2: expand
------------------------------------------------------------------------------

expand :: ScopeEnv -> ConfigExpr -> Either Err (Maybe (SweepVariant SweepValue))
expand env = \case
  CEScalar node -> node & SVScalar & Single & Just & Right
  CELoadStr pos _ contents -> Scalar pos (SStr contents) & SVScalar & Single & Just & Right
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
    traverse (expandZipAlternative env pos) items
      <&> concat
      <&> M.singleton key
      <&> Expand
      <&> Just
  CEProd pos items ->
    expand env (CEZip pos (freshKey (path env) pos) items)
  CEFileBarrier _ fp inner -> do
    includedEnv <- preScan (emptyScopeEnv fp) inner
    includedValue <- expand includedEnv inner
    pure (namespaceScopes "__include_" fp <$> includedValue)
  CEInherit pos baseFile base override -> do
    -- The inherited base owns its scopes.  Namespace them before merging so
    -- an equally named child scope is independent rather than accidentally
    -- zip-linked across the file boundary.
    baseEnv <- preScan (emptyScopeEnv baseFile) base
    baseValue <- expand baseEnv base
    overrideValue <- expand env override
    case (baseValue, overrideValue) of
      (Nothing, value) -> Right value
      (Just baseConfig, Nothing) -> Right (Just (namespaceInheritedScopes baseFile baseConfig))
      (Just baseConfig, Just overrideConfig) ->
        Just <$> merge pos (namespaceInheritedScopes baseFile baseConfig) overrideConfig
  where
    env' = incScopeDepth env
    expandElem e = preScan env' e >>= flip expand e

    singletonObj pos k p v = SVObject pos (M.singleton k (p, v))

expandZipAlternative :: ScopeEnv -> Pos -> ConfigExpr -> Either Err [SweepValue]
expandZipAlternative env pos item = do
  expanded <- expand (emptyScopeEnv (path env)) (CEArray pos [item])
  case expanded of
    Just (Single (SVArray _ values)) -> pure (IM.elems values)
    _ -> Left ("zip alternative did not expand to an array", pos, path env)

namespaceInheritedScopes :: FilePath -> SweepVariant a -> SweepVariant a
namespaceInheritedScopes = namespaceScopes "__inherit_"

namespaceScopes :: Text -> FilePath -> SweepVariant a -> SweepVariant a
namespaceScopes _ _ single@(Single _) = single
namespaceScopes prefix file (Expand scopes) =
  Expand (M.mapKeys (\scope -> prefix <> T.pack file <> "::" <> scope) scopes)

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
  | otherwise = mergedLocals & map (\sv -> fmap (sv `mergeDeep`) (Expand inEnv))
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
