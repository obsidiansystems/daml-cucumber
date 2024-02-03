{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language TypeApplications #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}
module Daml.Cucumber.LSP
  ( runTestLspSession
  , server
  , go
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Lens hiding (has)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Log
import Control.Monad.STM
import Daml.Cucumber.Log
import Daml.Cucumber.Utils
import Data.Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.Dependent.Sum
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Some
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.IO.Handle qualified as H
import Language.LSP.Protocol.Capabilities qualified as LSP
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types hiding (Hover, SemanticTokens)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Test
import NeatInterpolation (text)
import Reflex hiding (Request, Response)
import Reflex.Host.Headless
import Reflex.Process
import System.Directory
import System.Posix.Process
import System.Posix.Types
import qualified System.Process as Proc
import System.Which
import Text.HTML.TagSoup

data RanTest = RanTest
  { ranTestTraces :: [Text]
  , ranTestError :: Text
  }
  deriving (Eq, Ord, Show)

data TestResponse = TestResponse
  { testResponseScenarioFunctionName :: Text
  , testResponseResult :: TestResult
  }
  deriving (Eq, Ord, Show)

data TestResult
  = TestResultRan RanTest
  | TestResultDoesn'tCompile Text
  deriving (Eq, Ord, Show)

getCompileError :: TestResult -> Maybe Text
getCompileError (TestResultDoesn'tCompile reason) = Just reason
getCompileError _ = Nothing

getTracesAndErrors :: TestResult -> ([Text], Text)
getTracesAndErrors (TestResultRan (RanTest traces errors)) = (traces, errors)
getTracesAndErrors _ = ([], "")

_exampleTrace :: Text
_exampleTrace = "Transactions: Active contracts: Return value: {}Trace: \\\"Given a party\\\"  \\\"When the party creates contract X\\\"  \\\"Then Contract X is created\\\""

parseTraces :: Text -> [Text]
parseTraces input
  | T.isPrefixOf delim str =
    let
      without = T.drop (T.length delim) str

      (found, rest) = T.breakOn delim without

      restWithoutDelim = T.drop (T.length delim) rest
    in
    case T.null found of
      True -> parseTraces restWithoutDelim
      False -> found : parseTraces restWithoutDelim
  | T.null str = []
  | otherwise = parseTraces $ T.drop 1 str
  where
    delim = "\""
    str = T.strip input

instance FromJSON TestResponse where
  parseJSON = withObject "Test Response" $ \params -> do
    fname <- parseFunctionName params
    inner <- parseRan params <|> parseCompileFail params
    pure $ TestResponse fname inner
    where
      parseRan params = do
        contents <- params .: "contents"
        let
          tagText = extractData $ parseTags contents
          traces = parseTraces tagText

          (errors, _) = T.breakOn "Ledger time:" tagText

        pure $ TestResultRan $ RanTest traces errors

      parseCompileFail params = do
        note <- params .: "note"
        pure $ TestResultDoesn'tCompile $ extractNoteData $ parseTags note

      parseFunctionName params = do
        T.drop 1 . T.dropWhile (/= '=') . T.dropWhile (/= '&') <$> params .: "uri"

      extractData = innerText . dropWhile (~/= (TagOpen "div" [("class", "da-code transaction")] :: Tag Text))
      extractNoteData = innerText . dropWhile (~/= (TagOpen "span" [("class", "da-hl-warning")] :: Tag Text))

getRPC :: Response -> Maybe (RPC Value)
getRPC (Notification rpc) = Just rpc
getRPC (Response _ rpc) = Just rpc
getRPC _ = Nothing

getTestResponse :: RPC Value -> Maybe TestResponse
getTestResponse =
  parseMaybe parseJSON . rpcParams

setCwd :: FilePath -> Proc.CreateProcess -> Proc.CreateProcess
setCwd fp cp = cp { Proc.cwd = Just fp }

mkTestUri :: FilePath -> Text -> Text
mkTestUri fp funcName = "daml://compiler?file=" <> (uriPath fp) <> "&top-level-decl=" <> funcName

uriPath :: String -> Text
uriPath = T.replace "/" "%2F" . T.pack

tShow :: Show a => a -> Text
tShow = T.pack . show

makeReq :: Text -> Text
makeReq body =
  T.intercalate crlf
  [ "Content-Length: " <> tShow (T.length body)
  , ""
  , body
  ]

crlf :: Text
crlf = "\r\n"

data Request = Request
  { requestId :: Integer
  , requestRpc :: RPC Text
  }
  deriving (Eq, Show)

wrapRequest :: Request -> Text
wrapRequest (Request rid (RPC method params)) =
  [text|
    {
      "jsonrpc": "2.0",
      "id": $idStr,
      "method": "$method",
      "params": $params
    }
  |]
  where
    idStr = tShow rid

data RPC a = RPC
  { rpcMethod :: Text
  , rpcParams :: a
  }
  deriving (Eq, Show)

data Response
  = Init
  | Notification (RPC Value)
  | Response Integer (RPC Value)
  deriving (Eq, Show)

makePrisms ''Response

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    parseInit o <|> parseResponse o <|> parseNotification o
    where
      parseInit o = do
        _ :: Value <- o .: "result"
        pure Init

      parseRPC o = do
        RPC <$> o .: "method" <*> o .: "params"

      parseResponse o = do
        Response <$> o .: "id" <*> parseRPC o

      parseNotification o = do
        Notification <$> parseRPC o

damlPath :: FilePath
damlPath = $(staticWhich "daml")

data DamlIde t = DamlIde
  { damlIde_testResponses :: Dynamic t (Set TestResponse)
  , damlIde_allResponses :: Dynamic t [Response]
  , damlIde_exit :: Event t Text
  }

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

go :: FilePath -> Log IO ()
go p = do
  handler <- askLogHandler
  let
    log :: MonadIO m' =>  Text -> m' ()
    log = liftIO . flip runLoggingT handler . logDebug
  cwd <- liftIO $ canonicalizePath p
  liftIO $ runHeadlessApp $ do
    rec
      (hin, hout) <- server log cwd
      (init, rsp) <- client log cwd hin hout never
      shown <- performEvent $ ffor init $ \_ -> liftIO $ writeFile "x" "************** INIT *************"
    pure shown

client
  :: ( MonadIO m
     , TriggerEvent t m
     , Reflex t
     , MonadIO (Performable m)
     , PerformEvent t m
     )
  => (forall m'. MonadIO m' => Text -> m' ())
  -> FilePath
  -> H.Handle
  -> H.Handle
  -> Event t (Either () (Some Lsp))
  -> m (Event t (), Event t (DSum Lsp Identity))
client log cwd serverInWrite serverOutRead e = do
  (initE, writeInit) <- newTriggerEvent
  (rspE, writeRsp) <- newTriggerEvent
  -- Actions to run on the client. Left () shuts the client down.
  msgChan <- liftIO newTChanIO
  performEvent_ $ liftIO . atomically . writeTChan msgChan <$> e
  let session = do
        next <- liftIO $ atomically $ readTChan msgChan
        log $ "Running next LSP command..."
        case next of
          Left () -> pure ()
          Right lspApi -> liftIO . writeRsp =<< handleLsp lspApi
      session0 = do
        log "Initializing..."
        liftIO $ writeInit ()
        session
  log "Starting LSP client..."
  sessionThread <- liftIO $ forkIO $ runSessionWithHandles
    serverInWrite -- server input handle, used to send messages
    serverOutRead -- server output handle, used to receive responses
    (defaultConfig { logStdErr = True, logMessages = True })
    fullCaps
    cwd
    session0
  pure (initE, rspE)


server
  :: forall t m.
     ( PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadIO m
     , MonadIO (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , Reflex t
     )
  => (forall m'. MonadIO m' => Text -> m' ())
  -> FilePath
  -> m (H.Handle, H.Handle)
server log cwd = do
  let damlProc = setCwd cwd $ Proc.proc damlPath ["ide", "--debug", "--scenarios", "yes"]
  (input, writeInput) <- newTriggerEvent
  log "Starting LSP server..."
  proc <- createProcess damlProc (ProcessConfig input never)
  performEvent_ $ ffor input $ log . T.pack . show
  performEvent_ $ ffor (_process_stdout proc) $ log . T.decodeUtf8
  performEvent_ $ ffor (_process_stderr proc) $ log . T.decodeUtf8
  -- Handle used to get input from the client to the server process.
  (serverInRead, serverInWrite) <- liftIO Proc.createPipe
  -- Read the handle that the session writes to, and send the contents to the server
  -- via it's input event
  readerThread <- liftIO $ forkIO $ readHandle serverInRead $ writeInput . SendPipe_Message
  -- Handle used to get output from the server to the client process.
  (serverOutRead, serverOutWrite) <- liftIO Proc.createPipe
  -- Send output produced by the server to the handle that the session reads from
  performEvent_ $ ffor (_process_stdout proc) $ liftIO . BS.hPut serverOutWrite
  pure (serverInWrite, serverOutRead)
  where
    readHandle :: H.Handle -> (BS.ByteString -> IO x) -> IO ()
    readHandle h trigger = fix $ \continue -> do
      open <- H.hIsOpen h
      when open $ do
        isReadable <- H.hIsReadable h
        when isReadable $ do
          out <- BS.hGetSome h 32768
          if BS.null out
            then H.hClose h
            else void (trigger out) *> continue

damlIde
  :: ( PostBuild t m
     , MonadHold t m
     , MonadFix m
     , MonadIO m
     , MonadIO (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , Reflex t
     )
  => Handler IO (WithSeverity Text)
  -> FilePath
  -> Bool
  -> Event t Request
  -> m (DamlIde t)
damlIde logHandler cwd verbose rpcEvent = do
  let
    damlProc = setCwd cwd $ Proc.proc damlPath ["ide", "--debug", "--scenarios", "yes"]
    sendPipe = fmap (SendPipe_Message . T.encodeUtf8 . makeReq . wrapRequest) rpcEvent
    logDebug' = flip runLoggingT logHandler . logDebug . T.decodeUtf8
    logWarning' = flip runLoggingT logHandler . logWarning . T.decodeUtf8

  process <- createProcess damlProc (ProcessConfig sendPipe never)
  when verbose $ do
    performEvent_ $ ffor (_process_stdout process) $ liftIO . logDebug'
    performEvent_ $ ffor (_process_stderr process) $ liftIO . logWarning'

  let
    errorOutput = T.decodeUtf8 <$> _process_stderr process

    stdout = _process_stdout process

  lastError <- holdDyn "damlc exited unexpectedly" errorOutput
  rec
    buffer <- foldDyn ($) "" $ flip (<>) <$> stdout

    let dResponses = fst . parseBuffer <$> buffer


  pure $ DamlIde
    { damlIde_testResponses = Set.fromList . catMaybes . fmap (getTestResponse <=< getRPC) <$> dResponses
    , damlIde_allResponses = dResponses
    , damlIde_exit = current lastError <@ _process_exit process
    }

parseBuffer :: BS.ByteString -> ([Response], BS.ByteString)
parseBuffer bs = (catMaybes . fmap decodeStrict $ allOfEm, rest)
  where
    (allOfEm, rest) = allDelimitedBlocks bs

allDelimitedBlocks :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
allDelimitedBlocks "" = ([], "")
allDelimitedBlocks bs =
  case getDelimitedBlock bs of
    ("", rest) -> ([], rest)
    (x, rest) ->  let
      (found, newRest) = allDelimitedBlocks rest
      in
      (x : found, newRest)

getDelimitedBlock :: BS.ByteString -> (BS.ByteString, BS.ByteString)
getDelimitedBlock input = case bsSafeHead fromFirstCurly of
  Just '{' ->
    let
      count' = (findClosingDelimiter 1 0 $ BS.drop 1 fromFirstCurly) + 1
      result = BS.take count' fromFirstCurly

      hasEndCurly = maybe False (=='}') $ bsSafeLast result
    in
    case hasEndCurly of
       True -> (result, BS.drop (count' + BS.length prefix) input)
       _ -> ("", input)

  _ -> ("", fromFirstCurly)
  where
    prefix = BS.takeWhile (not . isACurly) input
    fromFirstCurly = BS.dropWhile (not . isACurly) input

    findClosingDelimiter :: Int -> Int -> BS.ByteString -> Int
    findClosingDelimiter 0 total _ = total
    findClosingDelimiter n total input' = case bsSafeHead input' of
      Just '{' ->
        findClosingDelimiter (n + 1) (total + 1) $ BS.drop 1 input'
      Just '}' ->
        findClosingDelimiter (n - 1) (total + 1) $ BS.drop 1 input'
      Nothing -> total
      _ -> findClosingDelimiter n (total + 1) $ BS.drop 1 input'

isACurly :: Char -> Bool
isACurly = (\x -> x == '{' || x == '}')

bsSafeHead :: BS.ByteString -> Maybe Char
bsSafeHead bs
  | BS.null bs = Nothing
  | otherwise = Just $ BS.head bs

bsSafeLast :: BS.ByteString -> Maybe Char
bsSafeLast bs
  | BS.null bs = Nothing
  | otherwise = Just $ BS.last bs

mkDidOpenUri :: Text -> Text -> RPC Text
mkDidOpenUri fileContents uri =
  RPC "textDocument/didOpen" params
  where
    params =
      [text|
        {
          "textDocument": {
            "uri": "$uri",
            "languageId": "",
            "version": 0,
            "text": "$fileContents"
          }
        }
      |]

mkInitPayload :: ProcessID -> RPC Text
mkInitPayload pid =
  RPC "initialize" params
  where
    params =
      [text|
        {
          "processId": $pidStr,
          "capabilities": {
            "window": {
              "showMessage": {
                "messageActionItem": {
                  "additionalPropertiesSupport": true
                }
              },
              "workDoneProgress": true,
              "showDocument": {
                "support": true
              }
            }
          }
        }
      |]
    pidStr = tShow pid

runTestLspSession
  :: FilePath
  -> FilePath
  -> Bool
  -> [Text]
  -> Log IO (Either Text (Map Text ([Text], Text)))
runTestLspSession cwd filepath verbose testNames = do
  logDebug $ "Generated test file is " <> T.pack filepath
  logDebug $ "Running lsp session in " <> T.pack cwd <> " ..."
  cwd' <- liftIO $ canonicalizePath cwd
  filepath' <- liftIO $ canonicalizePath filepath
  generatedFile <- liftIO $ T.intercalate "\\n" .  T.lines . T.replace "\"" "\\\"" . T.replace "\\" "\\\\" <$> T.readFile filepath'
  pid <- liftIO getProcessID
  let
    reqs = fmap (\(reqId, tname) -> (tname, Request reqId $ mkDidOpenUri "" $ mkTestUri filepath' tname)) $ zip [2..] testNames
    allReqs = ("Init", Request 0 (mkInitPayload pid)) : ("Compile", Request 1 (mkDidOpenUri generatedFile $ "file://" <> ("daml/Generated.daml"))) : reqs

  logHandler <- askLogHandler
  testResults <- liftIO $ runHeadlessApp $ do
    pb <- getPostBuild

    rec
      DamlIde currentResults currentResponses failed <- damlIde logHandler cwd' verbose sendReq

      let
        getNextRequest :: [(Text, Request)] -> [Response] -> Maybe Request
        getNextRequest rs responses = fmap snd $ safeHead $ filter (not . filterFunc) rs
          where
            filterFunc r = any (flip shouldBeRemoved r) responses

        nextReq = getNextRequest allReqs <$> currentResponses

        sendReq = fmapMaybe id $ leftmost [updated nextReq, current nextReq <@ pb]

        shouldBeRemoved :: Response -> (Text, Request) -> Bool
        shouldBeRemoved Init ("Init", _) = True
        shouldBeRemoved (Response resId _) (_, Request reqId _) = resId == reqId
        shouldBeRemoved _ _ = False

    let
      bounce :: Reflex t => (a -> Bool) -> Event t a -> Event t a
      bounce f ev =
        flip fmapMaybe ev $ \a -> case f a of
          True -> Just a
          False -> Nothing

      newResults = updated $ Set.toList <$> currentResults

      -- responses = fmap (fmapMaybe (preview _Notification)) $ updated currentResponses
      -- publishDiagnostics = fmap (fmap rpcParams . filter ((== "textDocument/publishDiagnostics") . rpcMethod)) responses
      -- errors = ffor publishDiagnostics $ \ds -> fforMaybe ds $ \case
      --   (Object d) -> Aeson.lookup "diagnostics" d >>= (\(Object diag) -> Aeson.lookup "message" diag) >>= \(String msg) -> if "error:" `T.isInfixOf` msg then pure msg else Nothing


    -- performEvent_ $ ffor (responses) $ liftIO . flip runLoggingT logHandler . logDebug . T.pack . show
    -- performEvent_ $ ffor (updated currentResponses) $ liftIO . flip runLoggingT logHandler . logDebug . T.pack . show
    -- performEvent_ $ ffor (publishDiagnostics) $ liftIO . flip runLoggingT logHandler . logDebug . T.pack . show
    -- performEvent_ $ ffor (errors) $ liftIO . flip runLoggingT logHandler . logDebug . T.pack . show

    pure $ leftmost
      [ fmap Right $ bounce ((== length reqs) . length) $ newResults
      , fmap (Left . ("Daml doesn't compile:\n" <>)) $  bounce (not . T.null) $ (T.intercalate "\n" . catMaybes . fmap (getCompileError . testResponseResult)) <$> newResults
      -- , fmap (Left . ("Daml doesn't compile:\n" <>)) $  bounce (not . T.null) $ (T.intercalate "\n") <$> errors
      , Left <$> failed
      ]
  pure $ fmap (mconcat . fmap (\(TestResponse name result) -> Map.singleton name $ getTracesAndErrors result)) testResults

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
