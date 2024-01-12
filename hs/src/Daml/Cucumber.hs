module Daml.Cucumber where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Daml.Cucumber.Parse
import Daml.Cucumber.Types
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Reflex
import Reflex.Host.Headless
import Reflex.Process
import Reflex.Process.Lines
import qualified System.Process as Proc
import System.Which
import System.Exit

data Opts = Opts
  { _opts_darPath :: FilePath
  , _opts_ledgerHost :: String
  , _opts_ledgerPort :: Int
  , _opts_featureFile :: FilePath
  , _opts_scriptName :: String
  }

writeFeatureJson :: FilePath -> Feature -> IO ()
writeFeatureJson path f = LBS.writeFile path (encode f)

daml :: FilePath
daml = $(staticWhich "daml")

-- daml script --dar .daml/dist/daml-app-0.0.1.dar  --ledger-host localhost --ledger-port 6865 --script-name "Cucumber:exampleRun" --input-file out.json --output-file result.json
runDamlScript
  :: ( TriggerEvent t m, PerformEvent t m
     , MonadHold t m
     , MonadIO m, MonadIO (Performable m)
     , MonadFix m
     )
  => Opts
  -> FilePath
  -> FilePath
  -> m (Process t ByteString ByteString)
runDamlScript opts input output = do
  let damlScriptCmd = Proc.proc daml
        [ "script"
        , "--dar", _opts_darPath opts
        , "--ledger-host", _opts_ledgerHost opts
        , "--ledger-port", show (_opts_ledgerPort opts)
        , "--script-name", _opts_scriptName opts
        , "--input-file", input
        , "--output-file", output
        ]
  createProcess damlScriptCmd (ProcessConfig never never)

runCucumberFeature
  :: Opts
  -> FilePath
  -> FilePath
  -> IO (ExitCode, Text, Either String [[Message]], Lines)
runCucumberFeature opts input output = do
  feat <- parseFeature $ _opts_featureFile opts
  case feat of
    Left err -> error $ "Unable to parse feature file: " <> show err
    Right f -> do
      liftIO $ LBS.writeFile input $ encode f
      runHeadlessApp $ do
        p <- runDamlScript opts input output
        stdoutLines <- newLines (_process_stdout p) (() <$ _process_exit p)
        stdout <- foldDyn (<>) mempty stdoutLines
        performEvent $ ffor (attach (current stdout) $ _process_exit p) $ \(out, ec) -> do
          outputMessages <- liftIO $ eitherDecode <$> LBS.readFile output
          pure (ec, (_feature_name f), outputMessages, out)
