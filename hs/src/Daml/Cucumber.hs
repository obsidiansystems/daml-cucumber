module Daml.Cucumber where

import Control.Monad.Extra
import Control.Monad.State (State, modify, runState)
import Daml.Cucumber.Daml.Parse
import Daml.Cucumber.LSP
import Daml.Cucumber.Parse
import Daml.Cucumber.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Traversable
import Reflex
import System.Directory
import System.FilePath hiding (hasExtension)
import System.IO
import Text.Casing

data Opts = Opts
  { _opts_directory :: FilePath
  , _opts_featureFile :: Maybe FilePath
  , _opts_damlSourceDir :: FilePath
  }

writeFeatureJson :: FilePath -> Feature -> IO ()
writeFeatureJson path f = LBS.writeFile path (Aeson.encode f)

-- A file like .daml counts as having the extension .daml even though that isn't correct!
-- thus this function:
hasExtension :: String -> FilePath -> Bool
hasExtension ext path =
  case splitExtension path of
    ("", _) -> False
    (_, foundExt) | foundExt == "." <> ext -> True
    _ -> False

findFilesRecursive :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFilesRecursive pred' dir = do
  allContents <- listDirectory dir
  let
    -- We don't want the dot daml folder
    contents = fmap (dir </>) $ filter pred' allContents
  (directories, files) <- partitionM (doesDirectoryExist) contents
  others <- mapM (findFilesRecursive pred') directories
  pure $ files <> mconcat others

runTestSuite :: Opts -> IO ()
runTestSuite (Opts folder mFeatureFile damlFolder) = do
  allFiles <- findFilesRecursive (/= ".daml") folder
  let
    extraPred = case mFeatureFile of
      Just ff -> (T.isInfixOf (T.pack ff) . T.pack)
      Nothing -> const True

    featureFiles = filter (\x -> extraPred x && hasExtension "feature" x) allFiles

    damlFiles = filter (hasExtension "daml") allFiles

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
             getResult s' = Map.lookup s' resultMap

          for_ steps $ \stp -> do
            let
              pretty = prettyPrintStep stp
            case getResult $ T.pack pretty of
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
  T.pack . toCamel . fromWords . T.unpack . _scenario_name

writeDamlScript :: FilePath -> DamlScript -> IO ()
writeDamlScript path state = do
  handle <- openFile path WriteMode

  let moduleName = T.pack (takeBaseName path)

  T.hPutStrLn handle $ "module " <> moduleName <> " where"

  T.hPutStrLn handle "import Cucumber"
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
    T.hPutStrLn handle $ "  " <> "_ <- runCucumber $ do"
    for_ body $ \action -> do
      T.hPutStrLn handle $ "    " <> action
    T.hPutStrLn handle $ "  pure ()"
  hClose handle

makeStepMapping :: DamlFile -> Map Step StepFunc
makeStepMapping file =
  mconcat $ fmapMaybe (\d -> flip Map.singleton (StepFunc (damlFilePath file) (definitionName d)) <$> definitionStep d) $ damlFileDefinitions file

prettyPrintStep :: Step -> String
prettyPrintStep (Step key body) = show key <> " " <> T.unpack body

getAllRequiredFeatureSteps :: Feature -> Set Step
getAllRequiredFeatureSteps =
  Set.fromList . mconcat . fmap _scenario_steps . _feature_scenarios

getAllDefinedSteps :: [Definition] -> Set Step
getAllDefinedSteps = Set.fromList . fmapMaybe definitionStep
