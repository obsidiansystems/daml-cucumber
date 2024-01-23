{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Daml.Cucumber.LSP where

import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.IO.Class
import Control.Monad.Fix
import Reflex.Host.Headless
import Reflex.Process
import Reflex.Process.Lines
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Process as Proc
import System.Posix.Process
import System.Posix.Types
import Reflex
import Reflex.Process
import System.IO
import qualified Data.Text.IO as T
import NeatInterpolation(text)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Text.HTML.TagSoup

-- Syntax error
-- method textDocument/publishDiagnostics
-- params.diagnostics [message uri]

-- Otherwise we get a responseContent that must be parsed for failure
-- the exception is in a message

-- Check if response content has failed or error, and also look for the completed steps!

-- If there is an error, we see a span with class da-hl-error
-- The results go into  div with class da-code transaction
-- What about auth errors?
-- There is a Trace: that has the steps that ran, so we can leverage that to know what step failed


data TestResult = TestResult
  { testResultScenarioFunctionName :: Text
  , testResultTraces :: [Text]
  }
  deriving (Show)

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
    otherDelim = "&quot;"

    str = T.strip input

instance FromJSON TestResult where
  parseJSON = withObject "Test Result" $ \o -> do
    params <- o .: "params"
    contents <- params .: "contents"
    uri <- params .: "uri"
    let
      tagText = extractData $ parseTags contents
      traces = parseTraces tagText

      funcName = T.drop 1 . T.dropWhile (/= '=') . T.dropWhile (/= '&') $ uri
    pure $ TestResult funcName traces
    where
       extractData = innerText . dropWhile (~/= (TagOpen "div" [("class", "da-code transaction")] :: Tag Text))

setCwd :: FilePath -> Proc.CreateProcess -> Proc.CreateProcess
setCwd fp cp = cp { Proc.cwd = Just fp }

mkTestUri :: FilePath -> Text -> Text
mkTestUri fp funcName = "daml://compiler?file=" <> (T.replace "/" "%2F" $ T.pack fp) <> "&top-level-decl=" <> funcName

runTestLspSession :: FilePath -> [Text] -> IO (Map Text [Text])
runTestLspSession filepath testNames = do
  pid <- getProcessID
  let
    reqs = fmap (\(reqId, r) -> makeReq $ mkDidOpenUri reqId $ mkTestUri filepath r) $ zip [1..] testNames
    allReqs = fmap makeReq $ (mkInitPayload 0 pid : reqs)

  testResults <- runHeadlessApp $ do
    pb <- getPostBuild
    rec
      gotResult <- damlIde $ fmapMaybe safeHead $ leftmost [updated remainingRequests]

      remainingRequests <- foldDyn ($) [] $ mergeWith (.) [ const allReqs <$ pb
                                                          , drop 1 <$ gotResult
                                                          ]

      results <- foldDyn (:) [] gotResult

    performEvent_ $ liftIO . putStrLn . ("SEND" <> ) . show . length <$> (updated remainingRequests)
    -- performEvent_ $ liftIO . putStrLn . ("UPDATED" <> ) . show <$> (fmapMaybe safeHead $ updated remainingRequests)

    let
      onlyPassIfDone results
        | all (\n -> any (\x -> testResultScenarioFunctionName x == n) results) testNames = Just results
        | otherwise = Nothing

    pure $ fmapMaybe onlyPassIfDone $ updated results
  putStrLn . show $ testResults
  pure $ mconcat $ fmap (\(TestResult name traces) -> Map.singleton name traces) testResults

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

-- TODO(skylar): Generate this!
testUri :: Text
testUri = "daml://compiler?file=.%2Fdaml%2FGenerated.daml&top-level-decl=aContractCanMaybeBeCreated";

damlIde :: (MonadFix m, MonadIO m, MonadIO (Performable m), PerformEvent t m, TriggerEvent t m, Reflex t) => Event t Text -> m (Event t TestResult)
damlIde rpcEvent = do
  -- pb <- getPostBuild
  let
    sendPipe = fmap (SendPipe_Message . T.encodeUtf8) rpcEvent

    damlProc = setCwd "../test" $ Proc.proc "daml" ["ide", "--debug", "--scenarios", "yes"]

  -- holdDyn [] bu
  process <- createProcess damlProc (ProcessConfig sendPipe never)

  let
    stdout = _process_stdout process

  -- dReady <- holdDyn False never
  performEvent_ $ liftIO . BS.putStrLn <$> stdout
  pure $ fmapMaybe (decode . LBS.dropWhile (/= '{') . LBS.fromStrict) stdout

mkDidOpenUri :: Int -> Text -> Text
mkDidOpenUri theId uri = do
  [text|
      {
          "jsonrpc": "2.0",
          "id": $idStr,
          "method": "textDocument/didOpen",
          "params": {
              "textDocument": {
                  "uri": "$uri",
                  "languageId": "",
                  "version": 0,
                  "text": ""
              }
          }
      }
  |]
  where
    idStr = tShow theId

mkInitPayload :: Int -> ProcessID -> Text
mkInitPayload theId pid =
  [text|
    {
      "jsonrpc": "2.0",
      "id": $idStr,
      "method": "initialize",
      "params": {
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
    }
  |]
  where
    pidStr = tShow pid
    idStr = tShow theId
