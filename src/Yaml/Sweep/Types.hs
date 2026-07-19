-- | Core types for the YAML sweep engine.
module Yaml.Sweep.Types
  ( ConfigExpr (..),
    PathStep (..),
    Path,
    ScopeEnv (..),
    ScopeEntry (..),
    Err,
    noPos,
    note,
    renderErr,
    emptyScopeEnv,
    incScopeDepth,
    scopeLookup,
    scopeInsert,
    scopeMember,
  )
where

import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.YAML (Node, Pos (..))

data ConfigExpr
  = CEScalar !(Node Pos)
  | CEObject !Pos !(Map (Node ()) (Pos, ConfigExpr))
  | CEArray !Pos ![ConfigExpr]
  | CEProd !Pos ![ConfigExpr]
  | CEZip !Pos !Text ![ConfigExpr]
  | CEKeyDecl !Text !Pos
  | CEFileBarrier !Pos !FilePath !ConfigExpr
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

-- | Scope environment tracking declared scopes, the current nesting
-- depth, and the file being processed.  'bindings' maps scope names to
-- their declarations; 'depth' is the current nesting level (used for
-- shadowing checks); 'path' is the file in which the current subtree
-- originates, used for error reporting.
data ScopeEnv = ScopeEnv
  { bindings :: !(Map Text ScopeEntry),
    depth    :: !Int,
    path     :: !FilePath
  }
  deriving (Show, Eq)

data ScopeEntry = ScopeEntry
  { seIsExplicit  :: !Bool,
    sePos         :: !Pos,
    seFile        :: !FilePath,
    seDepth    :: !Int
  }
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------------

-- | A structured error: (message, source position, source file).
type Err = (Text, Pos, FilePath)

-- | Placeholder position for errors without a natural source location.
noPos :: Pos
noPos = Pos {posByteOffset = 0, posCharOffset = 0, posLine = 0, posColumn = 0}

-- | Annotate a 'Maybe' with an error value to make it an 'Either'.
note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

-- | Render an 'Err' as a human-readable @file:line:col: message@ string.
renderErr :: Err -> String
renderErr (msg, pos, file) =
  [i|#{file}:#{posLine pos}:#{posColumn pos}: #{msg}|]

------------------------------------------------------------------------------
-- ScopeEnv helpers
------------------------------------------------------------------------------

emptyScopeEnv :: FilePath -> ScopeEnv
emptyScopeEnv fp = ScopeEnv {bindings = Map.empty, depth = 0, path = fp}

-- | Increment the nesting depth by one (bindings and path preserved).
incScopeDepth :: ScopeEnv -> ScopeEnv
incScopeDepth env = env {depth = depth env + 1}

scopeLookup :: Text -> ScopeEnv -> Maybe ScopeEntry
scopeLookup name env = Map.lookup name (bindings env)

scopeInsert :: Text -> ScopeEntry -> ScopeEnv -> ScopeEnv
scopeInsert name entry env = env {bindings = Map.insert name entry (bindings env)}

scopeMember :: Text -> ScopeEnv -> Bool
scopeMember name env = Map.member name (bindings env)
