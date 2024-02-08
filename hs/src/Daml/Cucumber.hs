module Daml.Cucumber
  ( Opts(..)
  , start
  , test
  ) where

import Control.Arrow (first)
import Control.Exception (SomeException(..), catch)
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
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
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
import Reflex.FSNotify
import Reflex.Host.Headless
import System.Console.ANSI
import System.Directory qualified as Dir
import System.Directory.Contents qualified as Dir
import System.Directory.Contents.Zipper qualified as Z
import System.Exit
import System.FSNotify qualified as FS
import System.FilePath hiding (hasExtension)
import System.IO
import System.Info qualified as Sys
import Text.Casing
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Pos qualified as Parsec
import Text.Printf
import System.Process

-- | daml-cucumber configuration
data Opts = Opts
  { _opts_featureSource :: NonEmpty FilePath
  , _opts_damlSource :: FilePath
  , _opts_allowMissing :: Bool
  , _opts_generateOnly :: Bool
  , _opts_verbose :: Bool
  , _opts_logLsp :: Bool
  , _opts_watch :: Bool
  }
  deriving (Show)

start :: Opts -> IO ()
start opts = do
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \stdoutHandler ->
    runLoggingT runner $ \msg -> case msgSeverity msg of
      Debug | _opts_verbose opts || _opts_logLsp opts -> stdoutHandler (renderLogMessage msg)
      Debug -> pure ()
      _ -> stdoutHandler (renderLogMessage msg)
  where
    runner = do
      case _opts_watch opts of
        True -> do
          logger <- askLogHandler
          liftIO $ runHeadlessApp $ do
            pb <- getPostBuild
            damlFiles <- watchDirectoryTree conf (_opts_damlSource opts <$ pb) $ \e ->
              (takeExtension (FS.eventPath e) == ".daml") && (takeFileName (FS.eventPath e) /= "Generated.daml")
            featureFiles <- mapM watchFeature (toList $ _opts_featureSource opts)
            let anyChange = mergeWith const $ damlFiles : featureFiles
            rec
              pb' <- delay 0.1 pb
              out <- performEvent $ ffor go $ \_ ->
                liftIO $ catch (runLoggingT (runTestSuite opts) logger) $ \SomeException {} -> return ()
              isReady <- holdDyn False $ leftmost
                [ True <$ out
                , False <$ go
                ]
              waitingForReload <- holdDyn False $ fmap fst $ attachPromptlyDyn (not <$> isReady) anyChange
              go <- debounce 0.25 $ leftmost
                [ (()<$) $ ffilter fst $ attachPromptlyDyn isReady anyChange
                , (()<$) $ ffilter id $ updated waitingForReload
                , pb'
                ]
            pure never
        False -> runTestSuite opts
    watchFeature f = do
      pb <- getPostBuild
      isDir <- liftIO $ Dir.doesDirectoryExist f
      if isDir
        then watchDirectoryTree conf (f <$ pb) $ \e -> takeExtension (FS.eventPath e) == ".feature"
        else watchDirectoryTree conf (takeDirectory f <$ pb) $ \e -> takeFileName (FS.eventPath e) == takeFileName f
    conf = FS.defaultConfig
        { FS.confWatchMode =
            -- On macOS, use the polling backend due to https://github.com/luite/hfsevents/issues/13
            if Sys.os == "darwin"
              then FS.WatchModePoll 200000
              else FS.WatchModeOS
        }


-- | Check whether a filepath has a particular extension (including ".")
hasExtension :: String -> FilePath -> Bool
hasExtension ext path = takeExtension path == ext

-- | List all files recursively from a given root path, but don't include
-- directories. Avoids symlink loops.
findFilesRecursive :: FilePath -> IO [FilePath]
findFilesRecursive dir =
  maybe [] toList <$> Dir.buildDirTree dir

findDamlSources :: FilePath -> IO [FilePath]
findDamlSources dir = do
  Dir.buildDirTree dir >>= \case
    Nothing -> pure []
    Just t ->
      let z = case Z.downTo ".daml" $ Z.zipped t of
            Nothing -> toList t
            Just dotdaml -> case Z.remove dotdaml of
              Nothing -> []
              Just z' -> toList $ Z.unzipped z'
      in pure $ filter (hasExtension ".daml") z

data Files = Files
  { files_feature :: [FilePath]
  , files_daml :: [FilePath]
  , files_damlyaml :: DamlYaml
  }

-- | Retrieve .daml and .feature files
getProjectFiles :: MonadIO m => FilePath -> NonEmpty FilePath -> Log m (Either String Files)
getProjectFiles damlSource featureSources = do
  damlFiles <- liftIO (findDamlSources damlSource)
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
getUsedContextTypes (DamlFile fp typesyns definitions) =
  foldr getContextTypeUse mempty (fmap getDefinitionType definitions <> fmap getTypeSynonymType typesyns)
  where
    getContextTypeUse t b = case t of
      (Reg (Type "Cucumber" (context:_))) -> Set.insert (fp, context) b
      _ -> b

getDefinitionType :: Definition -> TypeSig
getDefinitionType (Definition _ _ ts) = ts

getTypeSynonymType :: TypeSynonym -> TypeSig
getTypeSynonymType (TypeSynonym _ ts) = ts

getContextTypeDefinitionFile :: Text -> [DamlFile] -> Maybe FilePath
getContextTypeDefinitionFile typeName files =
  safeHead $ fmap damlFilePath $ filter filterFunc files
  where
    filterFunc (DamlFile _ _typesyns defs) = any isDefForType defs
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

test :: IO ()
test = do
  let opts = Opts
        ("../example" :| [])
        "../example"
        False
        False
        True
        False
        False
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \stdoutHandler ->
    runLoggingT (test opts) $ \msg -> case msgSeverity msg of
      Debug | _opts_verbose opts || _opts_logLsp opts -> stdoutHandler (renderLogMessage msg)
      Debug -> pure ()
      _ -> stdoutHandler (renderLogMessage msg)
  where
    test = runTestSuite

runTestSuite :: Opts -> Log IO ()
runTestSuite opts = do
  let Opts featureLocation damlLocation allowMissing generateOnly _ logLsp _ = opts
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
          logInfo "Running tests..."
          let
            testFile = (damlLocation </> damlYaml_source damlyaml </> "Generated.daml")
            shouldRunTests = (allowMissing || Set.null missingSteps) && not generateOnly
          case shouldRunTests of
            True -> do
              writeDamlScript testFile result
              (ec, stdout, stderr) <- liftIO $ readProcessWithExitCode damlPath ["test", "--files", testFile] ""
              let
                out = fmapMaybe (damlTestResultLine $ T.pack testFile) $ T.lines $ T.pack stdout
                toStepResults b s = if b
                  then map (,StepSucceeded) (_scenario_steps s)
                  else map (,StepFailed "") (_scenario_steps s)
                results :: Report = map (\f -> (f, map (\s ->
                  let fn = getScenarioFunctionName s
                  in (Left s, case lookup fn out of Just r -> toStepResults r s; _ -> [])) (_feature_scenarios f))) features
              logNotice $ renderReport results
              let success = testsSucceeded results
              logNotice $ reportSummary results
              case ec of
                ExitSuccess -> logInfo $ "All tests passed!"
                _ -> logExitFailure $ case out of
                  [] -> "Tests failed to run. Check for errors in your application code."
                  _ -> "Tests failed"
              -- result' <- runTestLspSession damlLocation testFile logLsp $ fmap damlFuncName $ damlFunctions result
              -- case result' of
              --   Left err -> logExitFailure ("LSP session failed: " <> err)
              --   Right testResults -> do
              --     success <- evaluateResults definedSteps testResults features
              --     when (not $ Set.null missingSteps) $ do
              --       let msg = T.pack $ unlines $
              --             "Missing steps:" : map prettyPrintStep (Set.toList missingSteps)
              --       logWarning msg
              --     when (not success) $ liftIO exitFailure
            False -> do
              if generateOnly
                then do
                  writeDamlScript testFile result
                  logInfo $ "Test script written to " <> T.pack testFile
                else do
                  logExitFailure $ T.pack $ unlines $
                    "Missing steps:" : map prettyPrintStep (toList missingSteps)

damlTestResultLine
  :: Text -- ^ Test file
  -> Text -- ^ daml test output line
  -> Maybe (Text, Bool) -- ^ Scenario function name and whether it passed
damlTestResultLine testFile ln =
  let
    strings = T.splitOn ":" ln
    fromResult txt = "ok" `T.isPrefixOf` T.strip txt
  in case strings of
      [srcFile, functionName, result] | srcFile == testFile ->
        Just $ (functionName, fromResult result)
      _ -> Nothing

evaluateResults :: MonadIO m => Set Step -> Map Text ([Text], Text) -> [Feature] -> Log m Bool
evaluateResults definedSteps testResults features = do
  let r = report definedSteps testResults features
  logNotice $ renderReport r
  let success = testsSucceeded r
  logNotice $ reportSummary r
  pure success

testsSucceeded :: Report -> Bool
testsSucceeded r = all (==StepSucceeded) $ fmap snd $ squash $ squash r

reportSummary :: Report -> Text
reportSummary r =
  let failures = filterReport (\case StepSucceeded -> False; _ -> True) r
      totalFeatures = length r
      numFailingFeatures = length failures
      scenarios = squash r
      failingScenarios = squash failures
      numFailingScenarios = length failingScenarios
      totalScenarios = length scenarios
      steps = squash scenarios
      failingSteps = squash failingScenarios
      numFailingSteps = length failingSteps
      totalSteps = length steps
      numberWidth = length (show totalSteps)
      show' = printf $ "%" <> show numberWidth <> "d"
      summaryLine prefix total fails =
        prefix <> show' (total - fails) <> " of " <> show' total <> " passed"
  in T.pack $ unlines
      [ if numFailingFeatures == 0 then "Tests Passed \9989" else "Tests Failed \10060"
      , "  " <> summaryLine "Features:  " totalFeatures numFailingFeatures
      , "  " <> summaryLine "Scenarios: " totalScenarios numFailingScenarios
      -- , "  " <> summaryLine "Steps:     " totalSteps numFailingSteps
      ]

squash :: [(a, [b])] -> [b]
squash = snd . foldMap (first $ const ())

filterReport :: (StepReport -> Bool) -> Report -> Report
filterReport f inputList =
    [ (a, bsWithMatchingD)
    | (a, bs) <- inputList
    , let bsWithMatchingD =
            [ (b, csWithMatchingD)
            | (b, cs) <- bs
            , let csWithMatchingD = [(c, d) | (c, d) <- cs, f d]
            , not (null csWithMatchingD)
            ]
    , not (null bsWithMatchingD)]

data StepReport = StepSucceeded | StepFailed Text | StepDidNotRun
  deriving (Show, Eq)

-- | We aren't using a Map here to preserve the ordering
type Report = [(Feature, [(Either Scenario Outline, [(Step, StepReport)])])]

report
  :: Set Step
  -> Map Text ([Text], Text)
  -> [Feature]
  -> Report
report definedSteps testResults features =
  let
    filterScenarios = filter (isScenarioRunnable definedSteps)
    resultMap = ffor features $ \f ->
      ( f
      , let scenarios = filterScenarios (_feature_scenarios f)
            scenariosMap = fmap (\s -> (Left s, getScenarioResults s)) scenarios
            outlines = filter (isScenarioRunnable definedSteps . _outline_scenario) (_feature_outlines f)
            outlinesMap = mconcat $ fmap (\o -> (,) (Right o) <$> getOutlineResults o) outlines
         in scenariosMap <> outlinesMap
      )
   in resultMap
  where
    getScenarioResults :: Scenario -> [(Step, StepReport)]
    getScenarioResults = getResults id id getScenarioFunctionName

    getOutlineResults :: Outline -> [[(Step, StepReport)]]
    getOutlineResults outline = mconcat $
      ffor (_outline_examples outline) $ \ex -> do
        (headerRow, values) <- maybeToList $ List.uncons (_examples_table ex)
        ffor (zip [(1::Int)..] values) $ \(index, vals) ->
          let formatStep = foldr (.) id $ fmap (\(h, v) step -> step { _step_body = T.replace ("<"<>h<>">") v (_step_body step) }) $ zip headerRow vals
           in getResults formatStep (_outline_scenario . snd) (uncurry getOutlineRowFunctionName) (index, outline)

    getResults :: (Step -> Step) -> (a -> Scenario) -> (a -> Text) -> a -> [(Step, StepReport)]
    getResults formatStep getScenario getFunctionName a =
      let
        Scenario _ steps = getScenario a
        fname = getFunctionName a
        errors = maybe "" snd $ Map.lookup fname testResults
        resultMap = scenarioResultMap a
        getResult = flip Map.lookup resultMap
      in
        flip map steps $ \stp -> (formatStep stp,) $
          case getResult (T.pack $ prettyPrintStep stp) of
            Just True -> StepSucceeded
            Just False -> StepFailed errors
            Nothing -> StepDidNotRun
      where
       scenarioResultMap s =
         Map.fromList $
           maybe [] (collateStepResults . fmap (T.drop (T.length "step:")) . filter (T.isPrefixOf "step:") . fst) $
             Map.lookup (getFunctionName s) testResults

renderReport :: Report -> Text
renderReport r = T.unlines $ fmap (T.unlines . fmap T.unlines) $
  ffor r $ \(f, ss) ->
    ([_feature_name f] :) $ ffor ss $ \(s, stps) ->
      (("  Scenario: " <> _scenario_name (either id _outline_scenario s)) :) $ ffor stps $ \(stp, result) ->
        ("    " <> (T.pack $ show $ _step_keyword stp) <> " " <> _step_body stp) <> renderResult result
  where
    renderResult = (" => " <>) . \case
      StepSucceeded -> successColor "OK"
      StepFailed err -> errorColor $ "FAILED" <> if T.null err then "" else "\n" <> renderStrict (formatError err)
      StepDidNotRun -> warningColor "SKIPPED"
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
  . T.replace ":" ": "

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
    for_ (filter (isScenarioRunnable definedSteps . _outline_scenario) $ _feature_outlines feature) $ \outline@(Outline examples scenario) -> do
      for_ examples $ \(Examples _name table) -> do
        case table of
          [] -> error "Expected at least the header row"
          (headerRow : values) -> do
            let valuesMap = Map.fromList . zip headerRow <$> values
            for_ (zip [(1::Int)..] valuesMap) $ \(index, value) -> do
              fnames <- fmap mconcat $ for (_scenario_steps scenario) $ \step -> do
                case Map.lookup step stepMapping of
                  Nothing -> pure []
                  Just (StepFunc _file fname) -> do
                    let parameterNames = parseStepParameterNames (_step_body step)
                        parameters = fmap (T.pack . show) $ catMaybes $ flip Map.lookup value <$> parameterNames
                        fnameAndArgs = T.unwords (fname : parameters)
                    pure [debug ("step:" <> T.pack (prettyPrintStep step)), fnameAndArgs, debug "step:pass"]
              let sname = getOutlineRowFunctionName index outline
              modify $ addFunction $ DamlFunc sname (debug ("scenario:"<> sname) : fnames) (_scenario_name scenario)
      pure ()
  pure ()

-- | Parse parameters in Scenario Outline Step
parseStepParameterNames :: Text -> [Text]
parseStepParameterNames str = do
  guard $ not $ T.null str
  let tagStart = T.drop 1 $ T.dropWhile (/= '<') str
  guard $ not $ T.null tagStart
  let tag' = T.takeWhile (/= '>') tagStart
  tag' : parseStepParameterNames (T.drop (T.length tag') tagStart)

debug :: Text -> Text
debug n = "debug \"" <> n <> "\""

getScenarioFunctionName :: Scenario -> Text
getScenarioFunctionName =
  T.pack . toCamel . fromWords . unwords . fmap (filter isAlphaNum) . words . T.unpack . _scenario_name

getOutlineRowFunctionName :: Int -> Outline -> Text
getOutlineRowFunctionName rowIndex outline =
  mconcat [getScenarioFunctionName (_outline_scenario outline), T.pack (show rowIndex)]

writeDamlScript :: MonadIO m => FilePath -> DamlScript -> m ()
writeDamlScript path state = liftIO $ do
  handle <- openFile path WriteMode

  let moduleName = T.pack (takeBaseName path)

  T.hPutStrLn handle $ "module " <> moduleName <> " where"

  T.hPutStrLn handle "import Cucumber"
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
  mconcat $ fforMaybe (damlFileDefinitions file) $ \d ->
    flip Map.singleton (StepFunc (damlFilePath file) (definitionName d)) <$> definitionStep d

prettyPrintStep :: Step -> String
prettyPrintStep (Step key body) = show key <> " " <> T.unpack body

getAllRequiredFeatureSteps :: Feature -> Set Step
getAllRequiredFeatureSteps f =
  Set.fromList $ mconcat $ mconcat
    [ _scenario_steps <$> _feature_scenarios f
    , _scenario_steps . _outline_scenario <$> _feature_outlines f
    ]

getAllDefinedSteps :: [Definition] -> Set Step
getAllDefinedSteps = Set.fromList . fmapMaybe definitionStep
