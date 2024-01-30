module Daml.Cucumber
  ( runTestSuite
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (State, modify, runState, runStateT)
import Daml.Cucumber.Daml.Parse
import Daml.Cucumber.LSP
import Daml.Cucumber.Parse
import Daml.Cucumber.Types
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Traversable
import Reflex
import System.Directory.Contents qualified as Dir
import System.Exit
import System.FilePath hiding (hasExtension)
import System.IO
import Text.Casing

hasExtension :: String -> FilePath -> Bool
hasExtension ext path = takeExtension path == ext

-- | List all files recursively from a given root path, but don't include
-- directories. Avoids symlink loops.
findFilesRecursive :: FilePath -> IO [FilePath]
findFilesRecursive dir =
  maybe [] toList <$> Dir.buildDirTree dir

runTestSuite :: FilePath -> Maybe FilePath -> FilePath -> Bool -> IO ()
runTestSuite folder mFeatureFile damlFolder allowMissing = do
  allFiles <- findFilesRecursive folder
  let
    relevantFeatureFile = case mFeatureFile of
      Just ff -> (T.isInfixOf (T.pack ff) . T.pack)
      Nothing -> const True

    featureFiles = filter
      (\x -> relevantFeatureFile x && hasExtension ".feature" x)
      allFiles

    damlFiles = filter (hasExtension ".daml") allFiles

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

    (_, result) = runState (generateDamlSource definedSteps stepMapping features) (DamlScript mempty mempty)
    testfile = (damlFolder </> "Generated.daml")

    shouldRunTests = allowMissing || Set.null missingSteps

  case shouldRunTests of
    True -> do
      writeDamlScript testfile result
      result' <- runTestLspSession folder testfile $ fmap damlFuncName $ damlFunctions result
      case result' of
        Left error -> do
          T.putStrLn $ "lsp session failed: " <> error
          exitFailure

        Right testResults -> do
          success <- evaluateResults definedSteps testResults features
          when (not $ Set.null missingSteps) $ do
            putStrLn "Missing steps:"
            for_ (Set.toList missingSteps) $ putStrLn . prettyPrintStep
          when (not success) $ exitFailure

    False -> do
      putStrLn "Missing steps:"
      for_ (Set.toList missingSteps) $ putStrLn . prettyPrintStep
      exitFailure

evaluateResults :: Set Step -> Map Text ([Text], Text) -> [Feature] -> IO Bool
evaluateResults definedSteps testResults features = do
  (_, success) <- flip runStateT True $ do
    for_ features $ \feature -> do
      for_ (filter (isScenarioRunnable definedSteps) $ _feature_scenarios feature) $ \s@(Scenario name steps) -> do
        liftIO $ T.putStrLn $ "Scenario: " <> name
        let
           fname = getScenarioFunctionName s

           errors = maybe "" snd $ Map.lookup fname testResults
           resultMap = Map.fromList $ maybe [] (collateStepResults . fmap (T.drop (T.length "step:")) . filter (T.isPrefixOf "step:") . fst ) $ Map.lookup fname testResults
           getResult s' = Map.lookup s' resultMap

        for_ steps $ \stp -> do
          let
            pretty = prettyPrintStep stp
          case getResult $ T.pack pretty of
            Just True -> liftIO $ putStrLn $ ("  " <> pretty <> " => OK")
            Just False ->  do
              modify (const False)
              liftIO $ do
                putStrLn $ "  " <> pretty <> " => FAILED"
                T.putStrLn $ "    " <> errors
            _ -> do
              modify (const False)
              liftIO $ putStrLn $ ("  " <> pretty <> " => DID NOT RUN")

  pure success

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
  , damlFuncScenarioName :: Text
  }
  deriving (Eq, Show)

addImport :: Text -> DamlScript -> DamlScript
addImport theImport state =
  state { damlImports = Set.insert theImport (damlImports state) }

addFunction :: DamlFunc -> DamlScript -> DamlScript
addFunction func state =
  state { damlFunctions = func : (damlFunctions state) }

generateDamlSource :: Set Step -> Map Step StepFunc -> [Feature] -> State DamlScript ()
generateDamlSource definedSteps stepMapping features = do
  for_ features $ \feature -> do
    for_ (filter (isScenarioRunnable definedSteps) $ _feature_scenarios feature) $ \scenario -> do
      fnames <- fmap mconcat $ for (_scenario_steps scenario) $ \step -> do
        let
          Just (StepFunc file fname) = Map.lookup step stepMapping
        modify $ addImport $ T.pack file
        pure [debug ("step:" <> (T.pack $ prettyPrintStep step)), fname, debug "step:pass"]
      let
        sname = getScenarioFunctionName scenario
      modify $ addFunction $ DamlFunc sname (debug ("scenario:"<> sname) : fnames) (_scenario_name scenario)
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
  for_ (damlFunctions state) $ \(DamlFunc name body scenarioName) -> do
    T.hPutStrLn handle "\n"
    T.hPutStrLn handle $ "-- | Scenario: " <> scenarioName
    T.hPutStrLn handle $ name <> ": " <> "Script ()"
    T.hPutStrLn handle $ name <> " = do"
    T.hPutStrLn handle $ "  " <> "_ <- runCucumber $ do"
    for_ body $ \action -> do
      T.hPutStrLn handle $ "    " <> action
    T.hPutStrLn handle $ "  pure ()"
  hClose handle

isScenarioRunnable :: Set Step -> Scenario -> Bool
isScenarioRunnable definedSteps scenario = all (flip Set.member definedSteps) (_scenario_steps scenario)

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
