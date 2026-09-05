module Main (main) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Data.Map.Strict qualified as Map
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Yaml.Sweep.Loader (DocSnapshot (..), loadAndCache)
import Yaml.Sweep.Lsp.Handler (handleDefinition, handleHover, lookupSnapshot, removeDiagnosticRoot, updateDiagnosticRoot, validateDocument)
import Yaml.Sweep.Lsp.Types (lspPositionToPos)
import Prelude hiding (modifyMVar, newMVar)

data ServerState = ServerState
  { ssDiagnostics :: MVar (Map LSP.NormalizedUri (Map LSP.NormalizedUri [LSP.Diagnostic]))
  }

handlers :: LSP.Handlers (LSP.LspM ServerState)
handlers =
  mconcat
    [ LSP.notificationHandler LSP.SMethod_Initialized (const pass),
      LSP.requestHandler LSP.SMethod_TextDocumentDefinition definitionHandler,
      LSP.requestHandler LSP.SMethod_TextDocumentHover hoverHandler,
      LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen didOpenHandler,
      LSP.notificationHandler LSP.SMethod_TextDocumentDidChange didChangeHandler,
      LSP.notificationHandler LSP.SMethod_TextDocumentDidSave didSaveHandler,
      LSP.notificationHandler LSP.SMethod_TextDocumentDidClose didCloseHandler
    ]

definitionHandler ::
  LSP.TRequestMessage LSP.Method_TextDocumentDefinition ->
  (Either (LSP.TResponseError LSP.Method_TextDocumentDefinition) (LSP.MessageResult LSP.Method_TextDocumentDefinition) -> LSP.LspM ServerState ()) ->
  LSP.LspM ServerState ()
definitionHandler msg responder = do
  let params = msg._params
      uri = params._textDocument._uri
      position = params._position
  result <- withVfsOverlay (\fileReader -> definitionAt fileReader uri position)
  responder (Right result)

definitionAt :: (FilePath -> IO (Maybe Text)) -> LSP.Uri -> LSP.Position -> IO (LSP.MessageResult LSP.Method_TextDocumentDefinition)
definitionAt fileReader uri position = case LSP.uriToFilePath uri of
  Nothing -> pure noDefinition
  Just file ->
    loadAndCache fileReader file <&> \case
      Left _ -> noDefinition
      Right cache -> case lookupSnapshot cache file of
        Nothing -> noDefinition
        Just DocSnapshot {..} ->
          handleDefinition cache file (lspPositionToPos dsSource position)
            & maybe noDefinition (LSP.InL . LSP.Definition . LSP.InL)
  where
    noDefinition = LSP.InR (LSP.InR LSP.Null)

hoverHandler ::
  LSP.TRequestMessage LSP.Method_TextDocumentHover ->
  (Either (LSP.TResponseError LSP.Method_TextDocumentHover) (LSP.MessageResult LSP.Method_TextDocumentHover) -> LSP.LspM ServerState ()) ->
  LSP.LspM ServerState ()
hoverHandler msg responder = do
  let params = msg._params
      uri = params._textDocument._uri
      position = params._position
  result <- withVfsOverlay (\fileReader -> hoverAt fileReader uri position)
  responder (Right result)

hoverAt :: (FilePath -> IO (Maybe Text)) -> LSP.Uri -> LSP.Position -> IO (LSP.MessageResult LSP.Method_TextDocumentHover)
hoverAt fileReader uri position = case LSP.uriToFilePath uri of
  Nothing -> pure noHover
  Just file ->
    loadAndCache fileReader file <&> \case
      Left _ -> noHover
      Right cache -> case lookupSnapshot cache file of
        Nothing -> noHover
        Just DocSnapshot {..} ->
          handleHover cache file (lspPositionToPos dsSource position)
            & maybe noHover LSP.InL
  where
    noHover = LSP.InR LSP.Null

didOpenHandler :: LSP.TNotificationMessage LSP.Method_TextDocumentDidOpen -> LSP.LspM ServerState ()
didOpenHandler msg = validateUri msg._params._textDocument._uri

didChangeHandler :: LSP.TNotificationMessage LSP.Method_TextDocumentDidChange -> LSP.LspM ServerState ()
didChangeHandler msg = validateUri msg._params._textDocument._uri

didSaveHandler :: LSP.TNotificationMessage LSP.Method_TextDocumentDidSave -> LSP.LspM ServerState ()
didSaveHandler msg = validateUri msg._params._textDocument._uri

didCloseHandler :: LSP.TNotificationMessage LSP.Method_TextDocumentDidClose -> LSP.LspM ServerState ()
didCloseHandler msg = do
  ServerState {..} <- LSP.getConfig
  let rootNormUri = LSP.toNormalizedUri msg._params._textDocument._uri
  toPublish <- liftIO $ modifyMVar ssDiagnostics (pure . removeDiagnosticRoot rootNormUri)
  for_ toPublish \(target, diags) ->
    publishDiagnostics (LSP.fromNormalizedUri target) diags

validateUri :: LSP.Uri -> LSP.LspM ServerState ()
validateUri uri = for_ (LSP.uriToFilePath uri) \file -> do
  ServerState {..} <- LSP.getConfig
  let rootNormUri = LSP.toNormalizedUri uri
  newDiagnostics <- withVfsOverlay (`validateDocument` file)
  let newTargetMap =
        Map.fromList
          [ (LSP.toNormalizedUri (LSP.filePathToUri diagnosticFile), diags)
          | (diagnosticFile, diags) <- Map.toList newDiagnostics
          ]
  toPublish <- liftIO $ modifyMVar ssDiagnostics (pure . updateDiagnosticRoot rootNormUri newTargetMap)
  for_ toPublish \(target, diags) ->
    publishDiagnostics (LSP.fromNormalizedUri target) diags

withVfsOverlay :: ((FilePath -> IO (Maybe Text)) -> IO a) -> LSP.LspM ServerState a
withVfsOverlay action = do
  env <- LSP.getLspEnv
  liftIO (action (readVirtualFile env))

readVirtualFile :: LSP.LanguageContextEnv ServerState -> FilePath -> IO (Maybe Text)
readVirtualFile env file =
  LSP.runLspT env do
    LSP.getVirtualFile (LSP.toNormalizedUri (LSP.filePathToUri file))
      <&> fmap VFS.virtualFileText

publishDiagnostics :: LSP.Uri -> [LSP.Diagnostic] -> LSP.LspM ServerState ()
publishDiagnostics _uri _diagnostics =
  LSP.sendNotification LSP.SMethod_TextDocumentPublishDiagnostics LSP.PublishDiagnosticsParams {..}
  where
    _version = Nothing

serverOptions :: LSP.Options
serverOptions = LSP.defaultOptions {LSP.optTextDocumentSync = Just textDocumentSyncOptions}

textDocumentSyncOptions :: LSP.TextDocumentSyncOptions
textDocumentSyncOptions = LSP.TextDocumentSyncOptions {..}
  where
    _openClose = Just True
    _change = Just LSP.TextDocumentSyncKind_Full
    _willSave = Just False
    _willSaveWaitUntil = Just False
    _save = Just (LSP.InL True)

serverDef :: IO (LSP.ServerDefinition ServerState)
serverDef = do
  ssDiagnostics <- newMVar Map.empty
  let serverState = ServerState {..}
  pure
    LSP.ServerDefinition
      { defaultConfig = serverState,
        configSection = "yaml-sweep",
        parseConfig = \_ _ -> Right serverState,
        onConfigChange = const pass,
        doInitialize = \env _ -> pure (Right env),
        staticHandlers = const handlers,
        interpretHandler = \env -> LSP.Iso (LSP.runLspT env) liftIO,
        options = serverOptions
      }

main :: IO ()
main = do
  def <- serverDef
  void (LSP.runServer def)
