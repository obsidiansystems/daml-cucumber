module Daml.Cucumber where

import Data.Foldable
import Control.Monad.Fix
import Control.Monad.IO.Class
import Daml.Cucumber.Parse
import Daml.Cucumber.Types
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Reflex
import Reflex.Host.Headless
import Reflex.Process
import Reflex.Process.Lines
import System.Process
import qualified System.Process as Proc
import System.Which
import System.Exit
import System.Directory
import System.FilePath

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
runDamlScript ::
  Opts
  -> FilePath
  -> IO ByteString
runDamlScript opts output = do
  let damlScriptCmd = Proc.proc daml
        [ "script"
        , "--dar", _opts_darPath opts
        , "--ledger-host", _opts_ledgerHost opts
        , "--ledger-port", show (_opts_ledgerPort opts)
        , "--script-name", _opts_scriptName opts
        , "--output-file", "/dev/stdout"
        ]
  output <- readCreateProcess damlScriptCmd ""
  pure $ BS.pack output

runTestSuite :: Opts -> IO ()
runTestSuite opts = do
  Right feature <- parseFeature $ _opts_featureFile opts
  scriptResult <- runDamlScript opts "dank.txt"
  BS.putStrLn scriptResult
  let
    Just decoded = Aeson.decode . LBS.fromStrict $ scriptResult
    resultMap = makeResultMap decoded

  for_ (_feature_scenarios feature) $ \scenario -> do
    T.putStrLn $ "Scenario => " <> _scenario_name scenario
    for_ (_scenario_steps scenario) $ \(Step k b) -> do
      putStr $  "    " <> show k <> " " <> T.unpack b <> ": "
      let
        result = Map.lookup (k, b) resultMap
      case result of
        Just (StepResult _ _ r) -> putStrLn $ "Result " <> show r
        Nothing -> putStrLn $ "Missing"

makeResultMap :: [StepResult] -> Map (Keyword, Text) StepResult
makeResultMap results =
  mconcat $ (\a@(StepResult k t _) -> Map.singleton (k,t) a) <$>  results
