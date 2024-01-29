module Daml.Cucumber where

import System.IO
import Text.Casing
import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Monad.Extra
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Daml.Cucumber.Parse
import Daml.Cucumber.Types
import Daml.Cucumber.Daml.Parse
import Daml.Cucumber.LSP
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
import Text.Encoding.Z (zEncodeString)
import Reflex
import Reflex.Host.Headless
import Reflex.Process
import Reflex.Process.Lines
import System.Process
import qualified System.Process as Proc
import System.Exit
import System.Directory
import System.FilePath hiding (hasExtension)
import Control.Monad.State

data Opts = Opts
  { _opts_directory :: FilePath
  , _opts_featureFile :: Maybe FilePath
  , _opts_damlSourceDir :: FilePath
  }

writeFeatureJson :: FilePath -> Feature -> IO ()
writeFeatureJson path f = LBS.writeFile path (encode f)

-- A file like .daml counts as having the extension .daml even though that isn't correct!
-- thus this function:
hasExtension :: String -> FilePath -> Bool
hasExtension ext path =
  case splitExtension path of
    ("", _) -> False
    (_, foundExt) | foundExt == "." <> ext -> True
    _ -> False

findFilesRecursive :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFilesRecursive pred dir = do
  allContents <- listDirectory dir
  let
    -- We don't want the dot daml folder
    contents = fmap (dir </>) $ filter pred allContents
  (directories, files) <- partitionM (doesDirectoryExist) contents
  others <- mapM (findFilesRecursive pred) directories
  pure $ files <> mconcat others

runTestSuite :: Opts -> IO ()
runTestSuite (Opts folder mFeatureFile damlFolder) = do
  files <- findFilesRecursive (/= ".daml") folder
  let
    extraPred = case mFeatureFile of
      Just ff -> (T.isInfixOf (T.pack ff) . T.pack)
      Nothing -> const True

    featureFiles = filter (\x -> extraPred x && hasExtension "feature" x) files

    damlFiles = filter (hasExtension "daml") files

  Right features <- sequenceA <$> for featureFiles parseFeature
  Just files <- sequenceA <$> for damlFiles parseDamlFile

  putStrLn "Found feature files"
  for_ featureFiles $ putStrLn . ("  " <>)
  putStrLn ""
  putStrLn "Found DAML files"
  for_ damlFiles $ putStrLn . ("  " <>)
  let
    definitions = mconcat $ fmap damlFileDefinitions files
    requiredSteps = mconcat $ fmap getAllRequiredFeatureSteps features
    definedSteps = getAllDefinedSteps definitions

    missingSteps = Set.difference requiredSteps definedSteps
    stepMapping = mconcat $ fmap makeStepMapping files

  case Set.null missingSteps of
    False -> do
      putStrLn "Cannot run features, missing steps:"
      for_ (Set.toList missingSteps) $ putStrLn . prettyPrintStep

    True -> do
      let
        (_, result) = runState (generateDamlSource stepMapping features) (DamlScript mempty mempty)
        testfile = (damlFolder </> "Generated.daml")
      writeDamlScript testfile result
      testResults <- runTestLspSession folder testfile $ fmap damlFuncName $ damlFunctions result

      for_ features $ \feature -> do
        for_ (_feature_scenarios feature) $ \s@(Scenario name steps) -> do
          T.putStrLn $ "Scenario: " <> name
          let
             fname = getScenarioFunctionName s

             errors = maybe "" snd $ Map.lookup fname testResults
             resultMap = Map.fromList $ maybe [] (collateStepResults . fmap (T.drop (T.length "step:")) . filter (T.isPrefixOf "step:") . fst ) $ Map.lookup fname testResults
             getResult s = Map.lookup s resultMap

          for_ steps $ \s -> do
            let
              pretty = prettyPrintStep s
              result = getResult $ T.pack pretty
            case result of
              Just True -> putStrLn $ ("  " <> pretty <> " => OK")
              Just False -> do
                putStrLn $ "  " <> pretty <> " => FAILED"
                T.putStrLn $ "    " <> errors
              _ -> putStrLn $ ("  " <> pretty <> " => DID NOT RUN")
      pure ()
  pure ()

collateStepResults :: [Text] -> [(Text, Bool)]
collateStepResults (name:"pass": rest) = (name, True) : collateStepResults rest
collateStepResults (name:rest) = (name, False) : collateStepResults rest
collateStepResults _ = []

data StepFunc = StepFunc
  { stepFile :: FilePath
  , stepFunctionName :: Text
  }
  deriving (Eq, Show)

data DamlScript = DamlScript
  { damlImports :: Set Text
  , damlFunctions :: [DamlFunc]
  }
  deriving (Eq, Show)

data DamlFunc = DamlFunc
  { damlFuncName :: Text
  , damlFuncBody :: [Text]
  }
  deriving (Eq, Show)

addImport :: Text -> DamlScript -> DamlScript
addImport theImport state =
  state { damlImports = Set.insert theImport (damlImports state) }

addFunction :: DamlFunc -> DamlScript -> DamlScript
addFunction func state =
  state { damlFunctions = func : (damlFunctions state) }

generateDamlSource :: Map Step StepFunc -> [Feature] -> State DamlScript ()
generateDamlSource stepMapping features = do
  for_ features $ \feature -> do
    for_ (_feature_scenarios feature) $ \scenario -> do
      fnames <- fmap mconcat $ for (_scenario_steps scenario) $ \step -> do
        let
          Just (StepFunc file fname) = Map.lookup step stepMapping
        modify $ addImport $ T.pack file
        pure [debug ("step:" <> (T.pack $ prettyPrintStep step)), fname, debug "step:pass"]
      let
        sname = getScenarioFunctionName scenario
      modify $ addFunction $ DamlFunc sname (debug ("scenario:"<> sname) : fnames)
  pure ()

debug :: Text -> Text
debug n = "debug \"" <> n <> "\""

getScenarioFunctionName :: Scenario -> Text
getScenarioFunctionName =
  T.pack . zEncodeString . toCamel . fromWords . T.unpack . _scenario_name

writeDamlScript :: FilePath -> DamlScript -> IO ()
writeDamlScript path state = do
  handle <- openFile path WriteMode

  let moduleName = T.pack (takeBaseName path)

  T.hPutStrLn handle $ "module " <> moduleName <> " where"

  T.hPutStrLn handle "import Tester"
  T.hPutStrLn handle "import StateT"
  T.hPutStrLn handle "import Daml.Script"

  for_ (Set.toList $ damlImports state) $ \theImport -> do
    T.hPutStrLn handle $ "import " <> T.pack (takeBaseName $ T.unpack theImport)
    pure ()

  T.hPutStrLn handle "\n"
  for_ (damlFunctions state) $ \(DamlFunc name body) -> do
    T.hPutStrLn handle "\n"
    T.hPutStrLn handle $ name <> ": " <> "Script ()"
    T.hPutStrLn handle $ name <> " = do"
    T.hPutStrLn handle $ "  " <> "_ <- runTester $ do"
    for_ body $ \action -> do
      T.hPutStrLn handle $ "    " <> action
    T.hPutStrLn handle $ "  pure ()"
  hClose handle

makeStepMapping :: DamlFile -> Map Step StepFunc
makeStepMapping file =
  mconcat $ fmapMaybe (\d -> flip Map.singleton (StepFunc (damlFilePath file) (definitionName d)) <$> definitionStep d) $ damlFileDefinitions file

prettyPrintStep :: Step -> String
prettyPrintStep (Step keyword body) = show keyword <> " " <> T.unpack body

getAllRequiredFeatureSteps :: Feature -> Set Step
getAllRequiredFeatureSteps =
  Set.fromList . mconcat . fmap _scenario_steps . _feature_scenarios

getAllDefinedSteps :: [Definition] -> Set Step
getAllDefinedSteps = Set.fromList . fmapMaybe definitionStep
