-- | Core types for the YAML sweep engine.
module Yaml.Sweep.Types
  ( ConfigExpr (..),
    PathStep (..),
    Path,
    ScopeIdentity (..),
    LocalScopeId (..),
    VariableIdentity (..),
    ScopeEnv (..),
    ScopeEntry (..),
    Err,
    noPos,
    note,
    renderErr,
    emptyScopeEnv,
    emptyLocalScopeEnv,
    explicitLocalScopeEnv,
    incScopeDepth,
    scopeLookup,
    scopeLookupLocal,
    scopeLookupGlobal,
    scopeInsert,
    scopeInsertGlobal,
    scopeMember,
    scopeName,
  )
where

import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.YAML (Node, Pos (..))

data ConfigExpr
  = CEScalar (Node Pos)
  | CEObject Pos (Map (Node ()) (Pos, ConfigExpr))
  | CEArray Pos [ConfigExpr]
  | CEZip Pos Text [ConfigExpr]
  | CEKeyDecl Text Pos
  | CEGlobalKeyDecl Text Pos
  | CEVarDecl Bool Text Pos ConfigExpr
  | CEVarRef Text Pos
  | CEOrigin FilePath ConfigExpr
  | CEFileBarrier Pos FilePath ConfigExpr
  | -- | An inherited base is expanded in its own scope and then deep-merged
    -- with the child expression.  Unlike an include, the child may contain a
    -- top-level sweep after inheritance.
    CEInherit Pos FilePath ConfigExpr ConfigExpr
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Path
------------------------------------------------------------------------------

data PathStep = PSKey Text | PSIdx Int
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
data ScopeIdentity
  = LocalScope LocalScopeId
  | GlobalScope Text
  deriving (Show, Eq, Ord)

data LocalScopeId
  = LexicalScope Text
  | DocumentScope FilePath Text
  | IncludedScope FilePath LocalScopeId
  | InheritedScope FilePath LocalScopeId
  deriving (Show, Eq, Ord)

data VariableIdentity
  = LocalVariable FilePath Text
  | GlobalVariable Text
  deriving (Show, Eq, Ord)

data ScopeEnv = ScopeEnv
  { localBindings :: Map Text ScopeEntry,
    globalBindings :: Map Text ScopeEntry,
    depth :: Int,
    path :: FilePath
  }
  deriving (Show, Eq)

data ScopeEntry = ScopeEntry
  { seIdentity :: ScopeIdentity,
    seIsExplicit :: Bool,
    sePos :: Pos,
    seFile :: FilePath,
    seDepth :: Int
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
emptyScopeEnv path = ScopeEnv {..}
  where
    localBindings = Map.empty
    globalBindings = Map.empty
    depth = 0

emptyLocalScopeEnv :: ScopeEnv -> ScopeEnv
emptyLocalScopeEnv ScopeEnv {..} = ScopeEnv {localBindings = Map.empty, ..}

explicitLocalScopeEnv :: ScopeEnv -> ScopeEnv
explicitLocalScopeEnv env@ScopeEnv {..} = env {localBindings = Map.filter seIsExplicit localBindings}

-- | Increment the nesting depth by one (bindings and path preserved).
incScopeDepth :: ScopeEnv -> ScopeEnv
incScopeDepth env = env {depth = depth env + 1}

scopeLookup :: Text -> ScopeEnv -> Maybe ScopeEntry
scopeLookup name ScopeEnv {..} = Map.lookup name localBindings <|> Map.lookup name globalBindings

scopeLookupLocal :: Text -> ScopeEnv -> Maybe ScopeEntry
scopeLookupLocal name ScopeEnv {..} = Map.lookup name localBindings

scopeLookupGlobal :: Text -> ScopeEnv -> Maybe ScopeEntry
scopeLookupGlobal name ScopeEnv {..} = Map.lookup name globalBindings

scopeInsert :: Text -> ScopeEntry -> ScopeEnv -> ScopeEnv
scopeInsert name entry env@ScopeEnv {..} = env {localBindings = Map.insert name entry localBindings}

scopeInsertGlobal :: Text -> ScopeEntry -> ScopeEnv -> ScopeEnv
scopeInsertGlobal name entry env@ScopeEnv {..} = env {globalBindings = Map.insert name entry globalBindings}

scopeMember :: ScopeIdentity -> ScopeEnv -> Bool
scopeMember scopeIdentity ScopeEnv {..} = case scopeIdentity of
  GlobalScope {} -> True
  LocalScope {} -> any ((== scopeIdentity) . seIdentity) localBindings

scopeName :: ScopeIdentity -> Text
scopeName = \case
  GlobalScope name -> name
  LocalScope localScope -> case localScope of
    LexicalScope name -> name
    DocumentScope _ name -> name
    IncludedScope _ inner -> scopeName (LocalScope inner)
    InheritedScope _ inner -> scopeName (LocalScope inner)
