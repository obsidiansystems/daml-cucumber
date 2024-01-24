{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Daml.Cucumber.LSP where

import Debug.Trace
import Data.Maybe
import Data.Map (Map)
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad.Fix
import Reflex.Host.Headless
import Reflex.Process
import Reflex.Process.Lines
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Process as Proc
import System.Posix.Process
import System.Posix.Types
import Reflex hiding (Request, Response)
import Reflex.Process
import System.IO
import qualified Data.Text.IO as T
import NeatInterpolation(text)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Text.HTML.TagSoup

data RanTest = RanTest
  { ranTestTraces :: [Text]
  , ranTestError :: Text
  }
  deriving (Show)

data TestResponse = TestResponse
  { testResponseScenarioFunctionName :: Text
  , testResponseResult :: TestResult
  }
  deriving (Show)

data TestResult
  = TestResultRan RanTest
  | TestResultDoesn'tCompile Text
  deriving (Show)

getTracesAndErrors :: TestResult -> ([Text], Text)
getTracesAndErrors (TestResultRan (RanTest traces errors)) = (traces, errors)
getTracesAndErrors _ = ([], "")

exampleTrace :: Text
exampleTrace = "Transactions: Active contracts: Return value: {}Trace: \\\"Given a party\\\"  \\\"When the party creates contract X\\\"  \\\"Then Contract X is created\\\""

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
      extractNoteData = innerText . dropWhile (~/= (TagOpen "div" [("class", "da-hl-warning")] :: Tag Text))

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
mkTestUri fp funcName = "daml://compiler?file=" <> (T.replace "/" "%2F" $ T.pack fp) <> "&top-level-decl=" <> funcName

runTestLspSession :: FilePath -> [Text] -> IO (Map Text ([Text], Text))
runTestLspSession filepath testNames = do
  pid <- getProcessID
  let
    reqs = fmap (\(reqId, tname) -> (tname, Request reqId $ mkDidOpenUri $ mkTestUri filepath tname)) $ zip [1..] testNames
    allReqs = ("Init", Request 0 (mkInitPayload pid)) : reqs

  testResults <- runHeadlessApp $ do
    pb <- getPostBuild
    rec
      response <- damlIde sendReq

      let
        sendReq = fmap snd $ fmapMaybe safeHead $ leftmost [updated remainingRequests]
        gotResults = catMaybes . fmap (getTestResponse <=< getRPC) <$> response

        shouldBeRemoved Init ("Init", _) = True
        shouldBeRemoved (Response resId _) (_, Request reqId _) = resId == reqId
        shouldBeRemoved _ _ = False

      remainingRequests <- foldDyn ($) [] $ mergeWith (.) [ const allReqs <$ pb
                                                          , mconcat . fmap (\x -> (filter (not . shouldBeRemoved x))) <$> response
                                                          ]

      results <- foldDyn (<>) [] gotResults

    let
      onlyPassIfDone results
        | all (\n -> any (\x -> testResponseScenarioFunctionName x == n) results) testNames = Just results
        | otherwise = Nothing

    pure $ fmapMaybe onlyPassIfDone $ updated results
  pure $ mconcat $ fmap (\(TestResponse name result) -> Map.singleton name $ getTracesAndErrors result) testResults

safeHead :: [a] -> Maybe a
safeHead (a:_) = Just a
safeHead _ = Nothing

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

damlIde :: (MonadHold t m, MonadFix m, MonadIO m, MonadIO (Performable m), PerformEvent t m, TriggerEvent t m, Reflex t) => Event t Request -> m (Event t [Response])
damlIde rpcEvent = do
  let
    damlProc = setCwd "../test" $ Proc.proc "daml" ["ide", "--debug", "--scenarios", "yes"]

  let
    sendPipe = fmap (SendPipe_Message . T.encodeUtf8 . makeReq . wrapRequest) rpcEvent

  process <- createProcess damlProc (ProcessConfig sendPipe never)
  let
    stdout = _process_stdout process

    thing f = case f of
      [] -> Nothing
      _ -> Just f

    result = fmapMaybe thing $ catMaybes . fmap (decode . LBS.fromStrict) . filter (not . BS.null) . fmap (BS.dropWhileEnd (/= '}') . BS.strip . BS.dropWhile (/= '{')) . BS.split '\n' <$> stdout
  pure result

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
