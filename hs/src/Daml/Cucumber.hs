module Daml.Cucumber
  ( Opts(..)
  , start
  ) where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Log
import Control.Monad.Log.Colors (wrapSGRCode)
import Control.Monad.State (State, modify, runState)
import Daml.Cucumber.Daml.Parse
import Daml.Cucumber.Daml.Yaml (DamlYaml(..))
import Daml.Cucumber.Daml.Yaml qualified as Yaml
import Daml.Cucumber.LSP
import Daml.Cucumber.Log
import Daml.Cucumber.Parse
import Daml.Cucumber.Types
import Daml.Cucumber.Utils
import Data.Char
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Traversable
import Prettyprinter
import Prettyprinter.Render.Text
import Prettyprinter.Util (reflow)
import Reflex
import System.Console.ANSI
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
  { _opts_featureSource :: NonEmpty FilePath
  , _opts_damlSource :: FilePath
  , _opts_allowMissing :: Bool
  , _opts_generateOnly :: Bool
  , _opts_verbose :: Bool
  }

start :: Opts -> IO ()
start opts = do
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \stdoutHandler ->
    runLoggingT (runTestSuite opts) $ \msg -> case msgSeverity msg of
      Debug | not (_opts_verbose opts) -> pure ()
      _ -> stdoutHandler (renderLogMessage msg)

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
getProjectFiles :: MonadIO m => FilePath -> NonEmpty FilePath -> m (Either String Files)
getProjectFiles damlSource featureSources = do
  damlFiles <- filter (hasExtension ".daml") <$>
    liftIO (findFilesRecursive damlSource)
  featureFiles <- filter (hasExtension ".feature") . concat <$>
    mapM (liftIO . findFilesRecursive) featureSources
  let damlyaml = damlSource </> "daml.yaml"
  yamlExists <- liftIO $ Dir.doesFileExist damlyaml
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

getUsedContextTypes :: DamlFile -> Set (FilePath, Text)
getUsedContextTypes (DamlFile fp definitions) =
  foldr getContextTypeUse mempty definitions
  where
    getContextTypeUse (Definition _ _ ts) b = case ts of
      (Reg (Type "Cucumber" (context:_))) -> Set.insert (fp, context) b
      _ -> b

getContextTypeDefinitionFile :: Text -> [DamlFile] -> Maybe FilePath
getContextTypeDefinitionFile typeName files =
  safeHead $ fmap damlFilePath $ filter filterFunc files
  where
    filterFunc (DamlFile _ defs) = any isDefForType defs

    isDefForType :: Definition -> Bool
    isDefForType (Definition name _ _) | typeName == name = True
    isDefForType _ = False

getContextType :: [DamlFile] -> Either Text Text
getContextType files = case (Set.toList $ mconcat $ fmap getUsedContextTypes files) of
  [] -> Left "No context type found"
  [(_, typeName)] -> Right typeName
  multipleContextTypes -> Left $ T.unlines $ "Test suite contains state that differs between steps" :
    (fmap (\(file, typeName) -> "  Type " <> typeName <> " in file " <> T.pack file) multipleContextTypes)

generateTest :: Files -> Log IO (Either Text Test)
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
      case mdamls of
        Nothing -> pure $ Left "Error parsing daml files" -- TODO add source information
        Just damls -> do
          let
            definitions = mconcat $ fmap damlFileDefinitions damls
            requiredSteps = mconcat $ fmap getAllRequiredFeatureSteps feats
            definedSteps = getAllDefinedSteps definitions
            missingSteps = Set.difference requiredSteps definedSteps
            stepMapping = mconcat $ fmap makeStepMapping damls
            _usedContextTypes = mconcat $ fmap getUsedContextTypes damls

          case getContextType damls of
            Right ctxType -> do
              let
                defaultDamlState = DamlScript mempty mempty

                initialState = case getContextTypeDefinitionFile ctxType damls of
                  Just file -> addImport (T.pack file) $ defaultDamlState
                  Nothing -> defaultDamlState

                (_, result) = runState (generateDamlSource definedSteps stepMapping feats) initialState
              pure $ Right $ Test
               { test_damlScript = result
               , test_defined = definedSteps
               , test_missing = missingSteps
               , test_features = feats
               }

            Left err -> pure $ Left err

logExitFailure :: MonadIO m => Text -> Log m ()
logExitFailure reason = do
  logError reason
  liftIO exitFailure

runTestSuite :: Opts -> Log IO ()
runTestSuite opts = do
  let Opts featureLocation damlLocation allowMissing generateOnly verbose = opts
  getProjectFiles damlLocation featureLocation >>= \case
    Left err -> logExitFailure $ T.pack err
    Right f@(Files featureFiles damlFiles damlyaml) -> do
      logDebug $ T.unlines $
        "Found feature files:" : map T.pack featureFiles
      logDebug $ T.unlines $
        "Found daml files:" : map T.pack damlFiles
      mscript <- generateTest f
      case mscript of
        Left err -> logExitFailure err
        Right (Test result definedSteps missingSteps features) -> do
          let
            testFile = (damlLocation </> damlYaml_source damlyaml </> "Generated.daml")
            shouldRunTests = (allowMissing || Set.null missingSteps) && not generateOnly
          case shouldRunTests of
            True -> do
              writeDamlScript testFile result
              result' <- runTestLspSession damlLocation testFile verbose $ fmap damlFuncName $ damlFunctions result
              case result' of
                Left err -> logExitFailure ("LSP session failed: " <> err)
                Right testResults -> do
                  success <- evaluateResults definedSteps testResults features
                  when (not $ Set.null missingSteps) $ do
                    let msg = T.pack $ unlines $
                          "Missing steps:" : map prettyPrintStep (Set.toList missingSteps)
                    if allowMissing
                      then logInfo msg
                      else logWarning msg
                  when (not success) $ logExitFailure "Test failures reported"
            False -> do
              if generateOnly
                then do
                  writeDamlScript testFile result
                  logInfo $ "Test script written to " <> T.pack testFile
                else do
                  logExitFailure $ T.pack $ unlines $
                    "Missing steps:" : map prettyPrintStep (toList missingSteps)

evaluateResults :: MonadIO m => Set Step -> Map Text ([Text], Text) -> [Feature] -> Log m Bool
evaluateResults definedSteps testResults features = do
  let r = report definedSteps testResults features
  logNotice $ renderReport r
  pure $ all (==StepSucceeded) $ fmap snd $ snd $ foldMap (first $ const ()) $ snd $ foldMap (first $ const ()) r

data StepReport = StepSucceeded | StepFailed Text | StepDidNotRun
  deriving (Eq)

-- | We aren't using a Map here to preserve the ordering
type Report = [(Feature, [(Scenario, [(Step, StepReport)])])]

report
  :: Set Step
  -> Map Text ([Text], Text)
  -> [Feature]
  -> Report
report definedSteps testResults features =
  let
    filterScenarios = filter (isScenarioRunnable definedSteps)
    scenarioMap = map (\f -> (f, filterScenarios $ _feature_scenarios f)) features
    resultMap = fmap (fmap (\scenarios -> map (\s -> (s, getResults s)) scenarios)) scenarioMap
  in resultMap
  where
    getResults :: Scenario -> [(Step, StepReport)]
    getResults s =
      let
        Scenario _ steps = s
        fname = getScenarioFunctionName s
        errors = maybe "" snd $ Map.lookup fname testResults
        resultMap = scenarioResultMap s
        getResult = flip Map.lookup resultMap
      in
        flip map steps $ \stp -> (stp,) $
          case getResult (T.pack $ prettyPrintStep stp) of
            Just True -> StepSucceeded
            Just False -> StepFailed errors
            Nothing -> StepDidNotRun
    scenarioResultMap :: Scenario -> Map Text Bool
    scenarioResultMap s =
      Map.fromList $
        maybe [] (collateStepResults . fmap (T.drop (T.length "step:")) . filter (T.isPrefixOf "step:") . fst) $
          Map.lookup (getScenarioFunctionName s) testResults

renderReport :: Report -> Text
renderReport r = T.unlines $ fmap (T.unlines . fmap T.unlines) $
  ffor r $ \(f, ss) ->
    ([_feature_name f] :) $ ffor ss $ \(s, stps) ->
      (("  Scenario: " <> _scenario_name s) :) $ ffor stps $ \(stp, result) ->
        ("    " <> (T.pack $ show $ _step_keyword stp) <> " " <> _step_body stp) <> renderResult result
  where
    renderResult = (" => " <>) . \case
      StepSucceeded -> successColor "OK"
      StepFailed err -> errorColor $ "FAILED\n" <> renderStrict (formatError err)
      StepDidNotRun -> warningColor "DID NOT RUN"
    errorColor = wrapSGRCode [SetColor Foreground Dull Red]
    warningColor = wrapSGRCode [SetColor Foreground Vivid Yellow]
    successColor = wrapSGRCode [SetColor Foreground Vivid Green]

-- | Remove extra internal whitespace, indent, and limit width
formatError :: Text -> SimpleDocStream ann
formatError = layoutSmart
  (defaultLayoutOptions
    { layoutPageWidth = AvailablePerLine 80 0.75
    })
  . indent 6
  . reflow
  . T.unwords
  . T.words

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

writeDamlScript :: MonadIO m => FilePath -> DamlScript -> m ()
writeDamlScript path state = liftIO $ do
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
