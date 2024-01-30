module Daml.Cucumber.LSP where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import NeatInterpolation (text)
import Reflex hiding (Request, Response)
import Reflex.Host.Headless
import Reflex.Process
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

runTestLspSession :: FilePath -> FilePath -> [Text] -> IO (Map Text ([Text], Text))
runTestLspSession cwd filepath testNames = do
  putStrLn $ "Running lsp session in " <> cwd
  putStrLn $ "Generated test file is " <> filepath
  pid <- getProcessID
  let
    reqs = fmap (\(reqId, tname) -> (tname, Request reqId $ mkDidOpenUri $ mkTestUri filepath tname)) $ zip [1..] testNames
    allReqs = ("Init", Request 0 (mkInitPayload pid)) : reqs

  testResults <- runHeadlessApp $ do
    pb <- getPostBuild

    rec
      (_response, currentResults, currentResponses) <- damlIde cwd sendReq

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

    pure $ bounce ((== length reqs) . length) $ updated $ Set.toList <$> currentResults
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

damlPath :: FilePath
damlPath = $(staticWhich "daml")

damlIde :: (PostBuild t m, MonadHold t m, MonadFix m, MonadIO m, MonadIO (Performable m), PerformEvent t m, TriggerEvent t m, Reflex t) => FilePath -> Event t Request -> m (Event t [Response], Dynamic t (Set TestResponse), Dynamic t [Response])
damlIde cwd rpcEvent = do
  let
    damlProc = setCwd cwd $ Proc.proc damlPath ["ide", "--debug", "--scenarios", "yes"]
    sendPipe = fmap (SendPipe_Message . T.encodeUtf8 . makeReq . wrapRequest) rpcEvent

  process <- createProcess damlProc (ProcessConfig sendPipe never)

  let
    stdout = _process_stdout process

    yieldIfNotEmpty f = case f of
      [] -> Nothing
      _ -> Just f

    yieldIfEmpty f = case f of
      [] -> Just ()
      _ -> Nothing

  rec
    buffer <- foldDyn ($) "" $ flip (<>) <$> stdout

    let
      parseResult = parseBuffer <$> updated buffer
      parsedResponses = fst <$> parseResult

      dResponses = fst . parseBuffer <$> buffer

      result = fmapMaybe yieldIfNotEmpty parsedResponses
      _hung = fmapMaybe yieldIfEmpty parsedResponses

  pure (result, Set.fromList . catMaybes . fmap (getTestResponse <=< getRPC) <$> dResponses, dResponses)

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
    -- findClosingDelimiter _ _ "" = 0
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
