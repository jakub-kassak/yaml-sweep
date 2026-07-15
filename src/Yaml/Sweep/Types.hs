{-# LANGUAGE DeriveAnyClass #-}

-- | Core types for the YAML sweep engine.
module Yaml.Sweep.Types
  ( SweepResult (..),
    ConfigExpr (..),
    PathStep (..),
    Path,
    ScopeEnv,
    ScopeEntry (..),
  )
where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Data.YAML (Pos)

data SweepResult = SweepResult
  { srValue :: Value
  }
  deriving (Show, Eq, Generic, NFData)

data ConfigExpr
  = CEScalar !Value
  | CEObject ![(Text, ConfigExpr)]
  | CEArray ![ConfigExpr]
  | CEProd !FilePath !Pos ![ConfigExpr]
  | CEZip !FilePath !Pos !Text ![ConfigExpr]
  | CEKeyDecl !Text !Pos !FilePath
  | CEFileBarrier !ConfigExpr
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Path
------------------------------------------------------------------------------

data PathStep = PSKey !Text | PSIdx !Int
  deriving (Show, Eq, Ord)

type Path = [PathStep]

------------------------------------------------------------------------------
-- Scope environment
------------------------------------------------------------------------------

type ScopeEnv = Map.Map Text ScopeEntry

data ScopeEntry = ScopeEntry
  { seIsExplicit  :: !Bool
  , sePos         :: !Pos
  , seScopeIdx    :: !Int
  }
  deriving (Show, Eq)
