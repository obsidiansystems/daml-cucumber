{-# Language PolyKinds #-}
module Daml.Cucumber.LSP
  ( runTestLspSession
  , damlPath
  ) where

import Prelude hiding (log)

import Control.Applicative
import Control.Applicative.Combinators (skipManyTill)
import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Log
import Daml.Cucumber.Log
import Daml.Cucumber.Utils
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Some
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types
import Language.LSP.Test
import NeatInterpolation (text)
import Network.URI (escapeURIString, isUnescapedInURIComponent)
import Reflex hiding (Request, Response)
import Reflex.Host.Headless
import Reflex.LSP
import Reflex.Process
import System.Directory
import System.Posix.Process
import System.Posix.Types
import qualified System.Process as Proc
import Text.HTML.TagSoup

#ifdef STATIC_WHICH
import System.Which
damlPath :: FilePath
damlPath = $(staticWhich "daml")
#else
damlPath :: FilePath
damlPath = "daml"
#endif

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
  pid <- liftIO getProcessID
  let
    reqs = fmap (\(reqId, tname) -> (tname, Request reqId $ mkDidOpenUri $ mkTestUri filepath' tname)) $ zip [1..] testNames
    allReqs = ("Init", Request 0 (mkInitPayload pid)) : reqs

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

    pure $ leftmost [ fmap Right $ bounce ((== length reqs) . length) $ newResults
                    , fmap (Left . ("Daml doesn't compile (are you missing a Default instance for your context type?):\n" <>)) $  bounce (not . T.null) $ (T.intercalate "\n" . catMaybes . fmap (getCompileError . testResponseResult)) <$> newResults
                    , Left <$> failed
                    ]
  pure $ fmap (mconcat . fmap (\(TestResponse name result) -> Map.singleton name $ getTracesAndErrors result)) testResults

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

data DamlIde t = DamlIde
  { damlIde_testResponses :: Dynamic t (Set TestResponse)
  , damlIde_allResponses :: Dynamic t [Response]
  , damlIde_exit :: Event t Text
  }

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

mkDidOpenUri :: Text -> RPC Text
mkDidOpenUri uri =
  RPC "textDocument/didOpen" params
  where
    params =
      [text|
        {
          "textDocument": {
            "uri": "$uri",
            "languageId": "",
            "version": 0,
            "text": ""
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

_test :: FilePath -> Log IO ()
_test p = do
  handler <- askLogHandler
  let
    log :: MonadIO m' =>  Text -> m' ()
    log = liftIO . flip runLoggingT handler . logDebug
  cwd <- liftIO $ canonicalizePath p
  liftIO $ runHeadlessApp $ do
    rec
      LspClient init rsp shutdown <- lsp $ LspClientConfig
        { _lspClientConfig_log = log
        , _lspClientConfig_workingDirectory = cwd
        , _lspClientConfig_serverCommand =
            Proc.RawCommand damlPath ["ide", "--debug", "--scenarios", "yes"]
        , _lspClientConfig_handler = handleDamlLsp
        , _lspClientConfig_requests = leftmost
          [ ffor init $ \_ -> Right
            [ Some $ DamlLsp_Lsp $ Lsp_Doc $ OpenDoc "daml/Main.daml" "daml"
            , Some $ DamlLsp_Lsp $ Lsp_Diagnostics $ WaitForDiagnostics
            , Some $ DamlLsp_OpenScenario "daml/Main.daml" "setup"
            , Some $ DamlLsp_WaitForScenarioDidChange
            ]
          , fforMaybe rsp $ \case
              (DamlLsp_WaitForScenarioDidChange :=> _) -> Just (Left ())
              _ -> Nothing
          ]
        }
    performEvent_ $ ffor rsp $ \(k :=> v) -> log . T.pack $ has @Show k $ show v
    pure shutdown

data DamlLsp a where
  DamlLsp_Lsp
    :: Lsp a -> DamlLsp a
  DamlLsp_OpenScenario
    :: FilePath -> String -> DamlLsp TextDocumentIdentifier
  DamlLsp_WaitForScenarioDidChange
    :: DamlLsp (Either VirtualResourceNoteSetParams VirtualResourceChangedParams)

handleDamlLsp :: Some DamlLsp -> Session (DSum DamlLsp Identity)
handleDamlLsp (Some req) = case req of
  DamlLsp_Lsp a -> do
    (k :=> v) <- handleLsp (Some a)
    pure $ DamlLsp_Lsp k :=> v
  DamlLsp_OpenScenario fp str -> (req :=>) . Identity <$> openScenario fp str
  DamlLsp_WaitForScenarioDidChange -> (req :=>) . Identity <$> waitForScenarioChangeOrNote

scenarioUri :: FilePath -> String -> Session Uri
scenarioUri fp name = do
    Just fp' <- uriToFilePath <$> getDocUri fp
    pure $ Uri $ T.pack $
        "daml://compiler?file=" <> escapeURIString isUnescapedInURIComponent fp' <>
        "&top-level-decl=" <> name

openScenario :: FilePath -> String -> Session TextDocumentIdentifier
openScenario fp name = do
    uri <- scenarioUri fp name
    sendNotification LSP.SMethod_TextDocumentDidOpen (DidOpenTextDocumentParams $ TextDocumentItem uri (T.pack "daml") 0 "")
    pure $ TextDocumentIdentifier uri

-- | Parameters for the virtual resource changed notification
data VirtualResourceChangedParams = VirtualResourceChangedParams
    { _vrcpUri      :: !T.Text
      -- ^ The uri of the virtual resource.
    , _vrcpContents :: !T.Text
      -- ^ The new contents of the virtual resource.
    } deriving Show

instance ToJSON VirtualResourceChangedParams where
    toJSON (VirtualResourceChangedParams uri contents) =
        object ["uri" .= uri, "contents" .= contents]

instance FromJSON VirtualResourceChangedParams where
  parseJSON = withObject "VirtualResourceChangedParams" $ \o ->
      VirtualResourceChangedParams <$> o .: "uri" <*> o .: "contents"

waitForScenarioChangeOrNote :: Session (Either VirtualResourceNoteSetParams VirtualResourceChangedParams)
waitForScenarioChangeOrNote = do
  skipManyTill anyMessage $ waitForScenarioDidChange <|> waitForVirtualResourceNote
  where
    waitForScenarioDidChange = do
      LSP.NotMess scenario <- customNotification $ Proxy @"daml/virtualResource/didChange"
      guard $ Lens.has (L.params . key "contents") scenario
      case fromJSON $ scenario ^. LSP.params of
          Success p -> pure $ Right p
          Data.Aeson.Types.Error s -> fail $ "Failed to parse daml/virtualResource/didChange params: " <> s
    waitForVirtualResourceNote = do
      LSP.NotMess note' <- skipManyTill anyMessage $
        customNotification $ Proxy @"daml/virtualResource/note"
      guard $ Lens.has (L.params . key "note") note'
      case fromJSON $ note' ^. LSP.params of
          Success p -> pure $ Left p
          Data.Aeson.Types.Error s -> fail $ "Failed to parse daml/virtualResource/note params: " <> s

-- | Parameters for the virtual resource changed notification
data VirtualResourceNoteSetParams = VirtualResourceNoteSetParams
    { _vrcpNoteUri      :: !T.Text
      -- ^ The uri of the virtual resource.
    , _vrcpNoteContent :: !T.Text
      -- ^ The new contents of the virtual resource.
    } deriving Show

instance ToJSON VirtualResourceNoteSetParams where
    toJSON (VirtualResourceNoteSetParams uri note) =
        object ["uri" .= uri, "note" .= note]

instance FromJSON VirtualResourceNoteSetParams where
    parseJSON = withObject "VirtualResourceNoteSetParams" $ \o ->
        VirtualResourceNoteSetParams <$> o .: "uri" <*> o .: "note"

deriveArgDict ''DamlLsp
