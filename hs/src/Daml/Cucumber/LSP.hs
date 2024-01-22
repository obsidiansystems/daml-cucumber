{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Daml.Cucumber.LSP where

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


data LspResponse = LspResponse
  { responseContent :: Text
  , repsponseUri :: Text
  }
  deriving (Show)

instance FromJSON LspResponse where
  parseJSON = withObject "LSP Response" $ \o -> do
    params <- o .: "params"
    contents <- params .: "contents"
    LspResponse (extractData (parseTags contents)) <$> params .: "uri"
    where
       extractData = innerText . dropWhile (~/= (TagOpen "div" [("class", "da-code transaction")] :: Tag Text))

setCwd :: FilePath -> Proc.CreateProcess -> Proc.CreateProcess
setCwd fp cp = cp { Proc.cwd = Just fp }

-- If there is an error, we see a span with class da-hl-error
-- The results go into  div with class da-code transaction
-- What about auth errors?
-- There is a Trace: that has the steps that ran, so we can leverage that to know what step failed

mkTestUri :: FilePath -> Text -> Text
mkTestUri fp funcName = "daml://compiler?file=" <> (T.replace "/" "%2F" $ T.pack fp) <> "&top-level-decl=" <> funcName

-- TODO(skylar):
-- Rework this whole thing
-- Also see if we can use a link to the file and get everything at once
runTestLspSession :: FilePath -> [Text] -> IO [LspResponse]
runTestLspSession filepath testNames = do
  pid <- getProcessID
  runHeadlessApp $ do
    pb <- getPostBuild
    delayedPb <- delay 0.1 pb

    rec
      dTests <- foldDyn ($) testNames $ drop 1 <$ gotResult
      gotResult <- damlIde $ leftmost [ (makeReq $ mkInitPayload pid) <$ pb
                                      , (makeReq . mkDidOpenUri . mkTestUri filepath) <$> (fmapMaybe safeHead $ leftmost [updated dTests, current dTests <@ delayedPb])
                                      ]


      results <- foldDyn (:) [] gotResult

    performEvent_ $ (liftIO $ putStrLn "updated!") <$ updated dTests

    let
      testsDone = ((length testNames ==) . length) <$> results

    performEvent_ $ (liftIO . putStrLn . show ) <$> updated testsDone

    pure $ gate (current testsDone) $ current results <@ updated testsDone

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

damlIde :: (MonadFix m, MonadIO m, MonadIO (Performable m), PerformEvent t m, TriggerEvent t m, Reflex t) => Event t Text -> m (Event t LspResponse)
damlIde rpcEvent = do
  let
    sendPipe = fmap (SendPipe_Message . T.encodeUtf8) rpcEvent

    damlProc = setCwd "../test" $ Proc.proc "daml" ["ide", "--debug", "--scenarios", "yes"]
  process <- createProcess damlProc (ProcessConfig sendPipe never)

  let
    stdout = _process_stdout process

  pure $ fmapMaybe (decode . LBS.dropWhile (/= '{') . LBS.fromStrict) stdout

mkDidOpenUri :: Text -> Text
mkDidOpenUri uri = do
  [text|
      {
          "jsonrpc": "2.0",
          "id": 1,
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

mkInitPayload :: ProcessID -> Text
mkInitPayload pid =
  [text|
    {
      "jsonrpc": "2.0",
      "id": 1,
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
