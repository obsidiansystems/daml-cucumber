module Reflex.LSP where

import Prelude hiding (log)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Constraint.Extras.TH
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Set (Set)
import Data.Some
import Data.Text (Text)
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types hiding (Hover, SemanticTokens)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Test
import Reflex
import System.Process

data Lsp :: * -> * where
  Lsp_Initialize :: Initialize a -> Lsp a
  Lsp_Doc :: Doc a -> Lsp a
  Lsp_Symbols :: Symbols a -> Lsp a
  Lsp_Diagnostics :: Diagnostics a -> Lsp a
  Lsp_Commands :: Commands a -> Lsp a
  Lsp_CodeActions :: CodeActions a -> Lsp a
  Lsp_Completions :: Completions a -> Lsp a
  Lsp_References :: References a -> Lsp a
  Lsp_Definitions :: Definitions a -> Lsp a
  Lsp_Renaming :: Renaming a -> Lsp a
  Lsp_Hover :: Hover a -> Lsp a
  Lsp_Highlights :: Highlights a -> Lsp a
  Lsp_Formatting :: Formatting a -> Lsp a
  Lsp_Edits :: Edits a -> Lsp a
  Lsp_CodeLenses :: CodeLenses a -> Lsp a
  Lsp_CallHierarchy :: CallHierarchy a -> Lsp a
  Lsp_SemanticTokens :: SemanticTokens a -> Lsp a
  Lsp_Capabilities :: Capabilities a -> Lsp a

deriving instance Show a => Show (Lsp a)

data Initialize :: * -> * where
  -- ^ Returns the initialize response that was received from the server. The
  -- initialize requests and responses are not included the session, so if you
  -- need to test it use this.
  InitializeResponse :: Initialize (LSP.TResponseMessage 'LSP.Method_Initialize)

deriving instance Show a => Show (Initialize a)

data Doc :: * -> * where
  -- | Creates a new text document. This is different from openDoc as it sends
  -- a workspace/didChangeWatchedFiles notification letting the server know
  -- that a file was created within the workspace, __provided that the server
  -- has registered for it__, and the file matches any patterns the server
  -- registered for. It does not actually create a file on disk, but is useful
  -- for convincing the server that one does exist.
  CreateDoc :: FilePath -> Text -> Text -> Doc TextDocumentIdentifier
  -- | Opens a text document that exists on disk, and sends a
  -- textDocument/didOpen notification to the server.
  OpenDoc :: FilePath -> Text -> Doc TextDocumentIdentifier
  -- | Closes a text document and sends a textDocument/didOpen notification to the server.
  CloseDoc :: TextDocumentIdentifier -> Doc ()
  -- | Changes a text document and sends a textDocument/didOpen notification to the server.
  ChangeDoc
    :: TextDocumentIdentifier
    -> [TextDocumentContentChangeEvent]
    -> Doc ()
  -- | The current text contents of a document.
  DocumentContents :: TextDocumentIdentifier -> Doc Text
  -- | Parses an ApplyEditRequest, checks that it is for the passed document and returns the new content
  GetDocumentEdit :: TextDocumentIdentifier -> Doc Text
  -- | Gets the Uri for the file corrected to the session directory.
  GetDocUri :: FilePath -> Doc Uri
  -- | Adds the current version to the document, as tracked by the session.
  GetVersionedDoc :: TextDocumentIdentifier -> Doc VersionedTextDocumentIdentifier

deriving instance Show a => Show (Doc a)

data Symbols :: * -> * where
  GetDocumentSymbols
    :: TextDocumentIdentifier
    -> Symbols (Either [SymbolInformation] [DocumentSymbol])

deriving instance Show a => Show (Symbols a)

data Diagnostics :: * -> * where
  -- | Waits for diagnostics to be published and returns them.
  WaitForDiagnostics :: Diagnostics [Diagnostic]
  -- | The same as waitForDiagnostics, but will only match a specific _source.
  WaitForDiagnosticsSource :: String -> Diagnostics [Diagnostic]
  -- | Expects a PublishDiagnosticsNotification and throws an
  -- UnexpectedDiagnostics exception if there are any diagnostics returned.
  NoDiagnostics :: Diagnostics ()
  -- | Returns the current diagnostics that have been sent to the client. Note
  -- that this does not wait for more to come in.
  GetCurrentDiagnostics :: TextDocumentIdentifier -> Diagnostics [Diagnostic]
  -- | Returns the tokens of all progress sessions that have started but not
  -- yet ended.
  GetIncompleteProgressSessions :: Diagnostics (Set ProgressToken)

deriving instance Show a => Show (Diagnostics a)

data Commands :: * -> * where
  -- | Executes a command.
  ExecuteCommand :: Command -> Commands ()

deriving instance Show a => Show (Commands a)

data CodeActions :: * -> * where
  -- | Returns the code actions in the specified range.
  GetCodeActions :: TextDocumentIdentifier -> Range -> CodeActions [Command |? CodeAction]
  -- | Returns the code actions in the specified range, resolving any with a non empty _data_ field.
  GetAndResolveCodeActions :: TextDocumentIdentifier -> Range -> CodeActions [Command |? CodeAction]
  -- | Returns all the code actions in a document by querying the code actions at each of the current diagnostics' positions.
  GetAllCodeActions :: TextDocumentIdentifier -> CodeActions [Command |? CodeAction]
  -- | Executes a code action. Matching with the specification, if a code action contains both an edit and a command, the edit will be applied first.
  ExecuteCodeAction :: CodeAction -> CodeActions ()
  -- | Resolves the provided code action.
  ResolveCodeAction :: CodeAction -> CodeActions CodeAction
  -- | If a code action contains a _data_ field: resolves the code action, then executes it. Otherwise, just executes it.
  ResolveAndExecuteCodeAction :: CodeAction -> CodeActions ()

deriving instance Show a => Show (CodeActions a)

data Completions :: * -> * where
  -- | Returns the completions for the position in the document.
  GetCompletions :: TextDocumentIdentifier -> Position -> Completions [CompletionItem]
  -- | Returns the completions for the position in the document, resolving any with a non empty _data_ field.
  GetAndResolveCompletions :: TextDocumentIdentifier -> Position -> Completions [CompletionItem]

deriving instance Show a => Show (Completions a)

data References :: * -> * where
  -- | Returns the references for the position in the document.
  GetReferences :: TextDocumentIdentifier -> Position -> Bool -> References [Location]

deriving instance Show a => Show (References a)

data Definitions a where
  -- | Returns the declarations(s) for the term at the specified position.
  GetDeclarations :: TextDocumentIdentifier -> Position -> Definitions (Declaration |? ([DeclarationLink] |? Null))
  -- | Returns the definition(s) for the term at the specified position.
  GetDefinitions :: TextDocumentIdentifier -> Position -> Definitions (Definition |? ([DefinitionLink] |? Null))
  -- | Returns the type definition(s) for the term at the specified position.
  GetTypeDefinitions :: TextDocumentIdentifier -> Position -> Definitions (Definition |? ([DefinitionLink] |? Null))
  -- | Returns the type definition(s) for the term at the specified position.
  GetImplementations :: TextDocumentIdentifier -> Position -> Definitions (Definition |? ([DefinitionLink] |? Null))

deriving instance Show a => Show (Definitions a)

data Renaming a where
  -- | Renames the term at the specified position.
  Rename :: TextDocumentIdentifier -> Position -> String -> Renaming ()

deriving instance Show a => Show (Renaming a)

data Hover a where
  -- | Returns the hover information at the specified position.
  GetHover :: TextDocumentIdentifier -> Position -> Hover (Maybe LSP.Hover)

deriving instance Show a => Show (Hover a)

data Highlights a where
  -- | Returns the highlighted occurrences of the term at the specified position
  GetHighlights :: TextDocumentIdentifier -> Position -> Highlights [DocumentHighlight]

deriving instance Show a => Show (Highlights a)

data Formatting a where
  -- | Applies formatting to the specified document.
  FormatDoc :: TextDocumentIdentifier -> FormattingOptions -> Formatting ()
  -- | Applies formatting to the specified range in a document.
  FormatRange :: TextDocumentIdentifier -> FormattingOptions -> Range -> Formatting ()

deriving instance Show a => Show (Formatting a)

data Edits a where
  -- | Applys an edit to the document and returns the updated document version.
  ApplyEdit :: TextDocumentIdentifier -> TextEdit -> Edits VersionedTextDocumentIdentifier

deriving instance Show a => Show (Edits a)

data CodeLenses a where
  -- | Returns the code lenses for the specified document.
  GetCodeLenses :: TextDocumentIdentifier -> CodeLenses [CodeLens]
  -- | Returns the code lenses for the specified document, resolving any with a non empty _data_ field.
  GetAndResolveCodeLenses :: TextDocumentIdentifier -> CodeLenses [CodeLens]
  -- | Resolves the provided code lens.
  ResolveCodeLens :: CodeLens -> CodeLenses CodeLens

deriving instance Show a => Show (CodeLenses a)

data CallHierarchy a where
  -- | Pass a param and return the response from prepareCallHierarchy
  PrepareCallHierarchy :: CallHierarchyPrepareParams -> CallHierarchy [CallHierarchyItem]
  IncomingCalls :: CallHierarchyIncomingCallsParams -> CallHierarchy [CallHierarchyIncomingCall]
  OutgoingCalls :: CallHierarchyOutgoingCallsParams -> CallHierarchy [CallHierarchyOutgoingCall]

deriving instance Show a => Show (CallHierarchy a)

data SemanticTokens a where
  -- | Pass a param and return the response from prepareCallHierarchy
  GetSemanticTokens :: TextDocumentIdentifier -> SemanticTokens (LSP.SemanticTokens |? Null)

deriving instance Show a => Show (SemanticTokens a)

data Capabilities a where
  -- | Returns a list of capabilities that the server has requested to dynamically register during the Session.
  GetRegisteredCapabilities :: Capabilities [LSP.SomeRegistration]

deriving instance Show a => Show (Capabilities a)

handleLsp :: Some Lsp -> Session (DSum Lsp Identity)
handleLsp (Some req) = (req :=>) . Identity <$> case req of
  Lsp_Initialize a -> case a of
    InitializeResponse -> initializeResponse
  Lsp_Doc doc -> case doc of
    CreateDoc path a b -> createDoc path a b
    OpenDoc path a -> openDoc path a
    CloseDoc a -> closeDoc a
    ChangeDoc a bs -> changeDoc a bs
    DocumentContents a -> documentContents a
    GetDocumentEdit a -> getDocumentEdit a
    GetDocUri path -> getDocUri path
    GetVersionedDoc a -> getVersionedDoc a
  Lsp_Symbols symbols -> case symbols of
    GetDocumentSymbols a -> getDocumentSymbols a
  Lsp_Diagnostics diag -> case diag of
    WaitForDiagnostics -> waitForDiagnostics
    WaitForDiagnosticsSource a -> waitForDiagnosticsSource a
    NoDiagnostics -> noDiagnostics
    GetCurrentDiagnostics a -> getCurrentDiagnostics a
    GetIncompleteProgressSessions -> getIncompleteProgressSessions
  Lsp_Commands cmd -> case cmd of
    ExecuteCommand a -> executeCommand a
  Lsp_CodeActions ca -> case ca of
    GetCodeActions i r -> getCodeActions i r
    GetAndResolveCodeActions i r -> getAndResolveCodeActions i r
    GetAllCodeActions i -> getAllCodeActions i
    ExecuteCodeAction a -> executeCodeAction a
    ResolveCodeAction a -> resolveCodeAction a
    ResolveAndExecuteCodeAction a -> resolveAndExecuteCodeAction a
  Lsp_Completions c -> case c of
    GetCompletions i p -> getCompletions i p
    GetAndResolveCompletions i p -> getAndResolveCompletions i p
  Lsp_References r -> case r of
    GetReferences i p b -> getReferences i p b
  Lsp_Definitions d -> case d of
    GetDeclarations i p -> getDeclarations i p
    GetDefinitions i p -> getDefinitions i p
    GetTypeDefinitions i p -> getTypeDefinitions i p
    GetImplementations i p -> getImplementations i p
  Lsp_Renaming r -> case r of
    Rename i p s -> rename i p s
  Lsp_Hover h -> case h of
    GetHover i p -> getHover i p
  Lsp_Highlights h -> case h of
    GetHighlights i p -> getHighlights i p
  Lsp_Formatting f -> case f of
    FormatDoc i o -> formatDoc i o
    FormatRange i o r -> formatRange i o r
  Lsp_Edits e -> case e of
    ApplyEdit i t -> applyEdit i t
  Lsp_CodeLenses cl -> case cl of
    GetCodeLenses i -> getCodeLenses i
    GetAndResolveCodeLenses i -> getAndResolveCodeLenses i
    ResolveCodeLens l -> resolveCodeLens l
  Lsp_CallHierarchy h -> case h of
    PrepareCallHierarchy p -> prepareCallHierarchy p
    IncomingCalls p -> incomingCalls p
    OutgoingCalls p -> outgoingCalls p
  Lsp_SemanticTokens st -> case st of
    GetSemanticTokens i -> getSemanticTokens i
  Lsp_Capabilities c -> case c of
    GetRegisteredCapabilities -> getRegisteredCapabilities

deriveArgDict ''Initialize
deriveArgDict ''Doc
deriveArgDict ''Symbols
deriveArgDict ''Diagnostics
deriveArgDict ''Commands
deriveArgDict ''CodeActions
deriveArgDict ''Completions
deriveArgDict ''References
deriveArgDict ''Definitions
deriveArgDict ''Renaming
deriveArgDict ''Hover
deriveArgDict ''Highlights
deriveArgDict ''Formatting
deriveArgDict ''Edits
deriveArgDict ''CodeLenses
deriveArgDict ''CallHierarchy
deriveArgDict ''SemanticTokens
deriveArgDict ''Capabilities
deriveArgDict ''Lsp

data LspClient t proto = LspClient
  { _lspClient_init :: Event t ()
  , _lspClient_responses :: Event t (DSum proto Identity)
  , _lspClient_shutdown :: Event t ()
  }

data LspClientConfig t proto = LspClientConfig
  { _lspClientConfig_log :: (forall m. MonadIO m => Text -> m ())
  , _lspClientConfig_workingDirectory :: FilePath
  , _lspClientConfig_serverCommand :: CmdSpec
  , _lspClientConfig_handler :: Some proto -> Session (DSum proto Identity)
  , _lspClientConfig_requests :: Event t (Either () [Some proto])
  }

lsp
  :: ( MonadIO m
     , TriggerEvent t m
     , Reflex t
     , MonadIO (Performable m)
     , PerformEvent t m
     )
  => LspClientConfig t proto
  -> m (LspClient t proto)
lsp (LspClientConfig log workingDirectory cmd handler e) = do
  (initE, writeInit) <- newTriggerEvent
  (rspE, writeRsp) <- newTriggerEvent
  (shutdownE, writeShutdown) <- newTriggerEvent
  -- Actions to run on the client. Left () shuts the client down.
  msgChan <- liftIO newTChanIO
  performEvent_ $ liftIO . atomically . writeTChan msgChan <$> e
  let session = do
        next <- liftIO $ atomically $ readTChan msgChan
        log $ "Running next LSP command..."
        case next of
          Right lspApis -> do
            forM_ lspApis $ liftIO . writeRsp <=< handler
            session
          Left () -> do
            log "Received shutdown request..."
            exitServer
            liftIO $ writeShutdown ()
      session0 = do
        log "Initializing..."
        liftIO $ writeInit ()
        session
  log "Starting LSP client..."
  sessionThread <- liftIO $ forkIO $ do
    runSessionWithConfigCustomProcess (\c -> c
      { cmdspec = cmd
      , cwd = Just workingDirectory
      })
      (defaultConfig { logStdErr = True, logMessages = True })
      ""
      fullCaps
      workingDirectory
      session0
  shutdown <- performEvent $ ffor shutdownE $ const $ liftIO $ killThread sessionThread
  pure $ LspClient initE rspE shutdown
  where
    exitServer = do
      request_ LSP.SMethod_Shutdown Nothing
      sendNotification LSP.SMethod_Exit Nothing
