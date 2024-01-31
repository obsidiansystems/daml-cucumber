module Daml.Cucumber
  ( runTestSuite
  , Opts(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (State, modify, runState, runStateT)
import Daml.Cucumber.Daml.Parse
import Daml.Cucumber.Daml.Yaml (DamlYaml(..))
import Daml.Cucumber.Daml.Yaml qualified as Yaml
import Daml.Cucumber.LSP
import Daml.Cucumber.Parse
import Daml.Cucumber.Types
import Data.Char
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
import System.Directory qualified as Dir
import System.Directory.Contents qualified as Dir
import System.Exit
import System.FilePath hiding (hasExtension)
import System.IO
import Text.Casing
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Pos qualified as Parsec

-- | daml-cucumber configuration
data Opts = Opts
  { _opts_featureSource :: FilePath
  , _opts_damlSource :: FilePath
  , _opts_allowMissing :: Bool
  , _opts_generateOnly :: Bool
  , _opts_verbose :: Bool
  }

-- | Check whether a filepath has a particular extension (including ".")
hasExtension :: String -> FilePath -> Bool
hasExtension ext path = takeExtension path == ext

-- | List all files recursively from a given root path, but don't include
-- directories. Avoids symlink loops.
findFilesRecursive :: FilePath -> IO [FilePath]
findFilesRecursive dir =
  maybe [] toList <$> Dir.buildDirTree dir

data Files = Files
  { files_feature :: [FilePath]
  , files_daml :: [FilePath]
  , files_damlyaml :: DamlYaml
  }

-- | Retrieve .daml and .feature files
getProjectFiles :: FilePath -> FilePath -> IO (Either String Files)
getProjectFiles damlSource featureSource = do
  damlFiles <- filter (hasExtension ".daml") <$>
    findFilesRecursive damlSource
  featureFiles <- filter (hasExtension ".feature") <$>
    findFilesRecursive featureSource
  let damlyaml = damlSource </> "daml.yaml"
  yamlExists <- Dir.doesFileExist damlyaml
  case (damlFiles, featureFiles, yamlExists) of
    ([], _, _) -> pure $ Left "No daml source files found"
    (_, [], _) -> pure $ Left "No feature files found"
    (_, _, False) -> pure $ Left "daml.yaml not found"
    _ -> do
      Yaml.parseDamlYaml damlyaml >>= pure . \case
        Left err -> Left $ Yaml.friendlyDamlYamlError err
        Right yml -> Right $ Files
          { files_feature = featureFiles
          , files_daml = damlFiles
          , files_damlyaml = yml
          }

data Test = Test
  { test_damlScript :: DamlScript
  , test_defined :: Set Step
  , test_missing :: Set Step
  , test_features :: [Feature]
  }

generateTest :: Files -> IO (Either Text Test)
generateTest f = do
  mfeats <- sequenceA <$> for (files_feature f) parseFeature
  case mfeats of
    Left err -> pure $ Left $ T.unlines $
      [ "Error parsing feature files:"
      , let pos = Parsec.errorPos err
        in T.pack $ (Parsec.sourceName pos) <> ": " <> show (Parsec.sourceLine pos)
      ] <> map (T.pack . Parsec.messageString) (Parsec.errorMessages err)
    Right feats -> do
      mdamls <- sequenceA <$> for (files_daml f) parseDamlFile
      pure $ case mdamls of
        Nothing -> Left "Error parsing daml files" -- TODO add source information
        Just damls ->
          let
            definitions = mconcat $ fmap damlFileDefinitions damls
            requiredSteps = mconcat $ fmap getAllRequiredFeatureSteps feats
            definedSteps = getAllDefinedSteps definitions
            missingSteps = Set.difference requiredSteps definedSteps
            stepMapping = mconcat $ fmap makeStepMapping damls
            (_, result) = runState (generateDamlSource definedSteps stepMapping feats) (DamlScript mempty mempty)
          in
            Right $ Test
              { test_damlScript = result
              , test_defined = definedSteps
              , test_missing = missingSteps
              , test_features = feats
              }

runTestSuite :: Opts -> IO ()
runTestSuite opts = do
  let Opts featureLocation damlLocation allowMissing generateOnly verbose = opts
  getProjectFiles damlLocation featureLocation >>= \case
    Left err -> do
      putStrLn err
      exitFailure
    Right f@(Files featureFiles damlFiles damlyaml) -> do
      when verbose $ do
        putStrLn "Found feature files"
        for_ featureFiles $ putStrLn . ("  " <>)
        putStrLn ""
        putStrLn "Found DAML files"
        for_ damlFiles $ putStrLn . ("  " <>)
      mscript <- generateTest f
      case mscript of
        Left err -> do
          putStrLn $ T.unpack err
          exitFailure
        Right (Test result definedSteps missingSteps features) -> do
          let
            testFile = (damlLocation </> damlYaml_source damlyaml </> "Generated.daml")
            shouldRunTests = (allowMissing || Set.null missingSteps) && not generateOnly
          case shouldRunTests of
            True -> do
              writeDamlScript testFile result
              result' <- runTestLspSession damlLocation testFile verbose $ fmap damlFuncName $ damlFunctions result
              case result' of
                Left err -> do
                  T.putStrLn $ "lsp session failed: " <> err
                  exitFailure
                Right testResults -> do
                  success <- evaluateResults definedSteps testResults features
                  when (not $ Set.null missingSteps) $ do
                    putStrLn "Missing steps:"
                    for_ (Set.toList missingSteps) $ putStrLn . prettyPrintStep
                  when (not success) $ exitFailure
            False -> do
              if generateOnly
                then do
                  writeDamlScript testFile result
                  putStrLn $ "Test script written to " <> testFile
                else do
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
  T.pack . toCamel . fromWords . unwords . fmap (filter isAlphaNum) . words . T.unpack . _scenario_name

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
