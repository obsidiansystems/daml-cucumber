module Daml.Cucumber
  ( Opts(..)
  , start
  , runWithLogger
  ) where

import Control.Arrow (first)
import Control.Exception (SomeException(..), catch)
import Control.Lens ((^.), _2, _3)
import Control.Monad
import Control.Monad.Catch (MonadMask)
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
import Data.String (IsString)
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
import System.Process
import Text.Casing
import Text.Parsec.Error qualified as Parsec
import Text.Parsec.Pos qualified as Parsec
import Text.Printf

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

-- | Run an action in a context that allows logging
runWithLogger
  :: (MonadIO m, MonadMask m, Monoid a, Pretty a, IsString a)
  => Bool
  -- ^ Verbose (enable debug logging)
  -> LoggingT (WithSeverity a) m b
  -- ^ Action to run with logging capabilities
  -> m b
runWithLogger verbose go = do
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \stdoutHandler ->
    runLoggingT go $ \msg -> case msgSeverity msg of
      Debug | verbose -> stdoutHandler (renderLogMessage msg)
      Debug -> pure ()
      _ -> stdoutHandler (renderLogMessage msg)

-- | Start daml-cucumber
start :: Opts -> IO ()
start opts = do
  let verbose = _opts_verbose opts || _opts_logLsp opts
  runWithLogger verbose runner
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

_test :: IO ()
_test = do
  let opts = Opts
        ("../example" :| [])
        "../example"
        False
        False
        True
        False
        False
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \stdoutHandler ->
    runLoggingT (test' opts) $ \msg -> case msgSeverity msg of
      Debug | _opts_verbose opts || _opts_logLsp opts -> stdoutHandler (renderLogMessage msg)
      Debug -> pure ()
      _ -> stdoutHandler (renderLogMessage msg)
  where
    test' = runTestSuite

runTestSuite :: Opts -> Log IO ()
runTestSuite opts = do
  let Opts featureLocation damlLocation allowMissing generateOnly _ logLsp _ = opts
  getProjectFiles damlLocation featureLocation >>= \case
    Left err -> logExitFailure $ T.pack err
    Right feat@(Files featureFiles damlFiles damlyaml) -> do
      logDebug $ T.unlines $
        "Found feature files:" : map T.pack featureFiles
      logDebug $ T.unlines $
        "Found daml files:" : map T.pack damlFiles
      mscript <- generateTest feat
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
              (ec, damlTestStdout, _) <- liftIO $ readProcessWithExitCode damlPath ["test", "--files", testFile] ""
              let
                out = fmapMaybe (damlTestResultLine $ T.pack testFile) $ T.lines $ T.pack damlTestStdout
                toStepResults fn b s = if b
                  then map (,fn,StepSucceeded) (_scenario_steps s)
                  else map (,fn,StepFailed "") (_scenario_steps s)

                outlineFormatStep :: Outline -> (Step -> Step)
                outlineFormatStep outline = foldr (.) id $ mconcat $ mconcat $
                  ffor (_outline_examples outline) $ \ex -> do
                    (headerRow, values) <- maybeToList $ List.uncons (_examples_table ex)
                    ffor (zip [(1::Int)..] values) $ \(_index, vals) ->
                      let exampleData = zip headerRow vals
                      in fmap (\(h, v) step -> step { _step_body = T.replace ("<"<>h<>">") v (_step_body step) }) exampleData

                formatOutlineSteps :: (Feature, [(Either Scenario Outline, [(Step, Text, StepReport)])]) -> (Feature, [(Either Scenario Outline, [(Step, Text, StepReport)])])
                formatOutlineSteps (f, scenarios) = (,) f $ ffor scenarios $ \case
                  (Right outline, steps) ->
                    let formatStep = outlineFormatStep outline
                     in (Right outline, fmap (\(s,b,c) -> (formatStep s, b, c)) steps)
                  xs -> xs

                damlTestResults :: Report = map (\f ->
                  ( f
                  , map (\s ->
                          let fn = getScenarioFunctionName s
                          in (Left s, case lookup fn out of Just r -> toStepResults fn r s; _ -> []))
                        (_feature_scenarios f)
                    <>
                      mconcat
                      (catMaybes
                       (map (\outline@(Outline examples scenario) -> do
                         (example, _) <- List.uncons examples
                         (_header, exData) <- List.uncons (_examples_table example)
                         Just $ flip fmap (zip [(1::Int)..] exData) $ \(index, _) ->
                           let fn = getOutlineRowFunctionName index outline
                               formatStep = outlineFormatStep outline
                           in ( Right outline
                              , case lookup fn out of
                                  Just r -> fmap (\(s, a, b) -> (formatStep s, a, b)) $ toStepResults fn r scenario
                                  Nothing -> []
                              ))
                        (_feature_outlines f)))
                  )) features
                failedThings = squash $ testFailures damlTestResults
                functionsToRerun :: [Text] = Set.toList $ Set.fromList $ fmap (^._2) $ concatMap snd $ failedThings
              case (ec, damlTestResults) of
                (ExitFailure _, []) -> logExitFailure "Tests failed to run. Please check for errors in your daml application code."
                _ -> do
                  let
                    finish r = do
                      logNotice $ renderReport r
                      let success = testsSucceeded r
                      logNotice $ reportSummary r
                      when (not success) $ liftIO exitFailure
                  case functionsToRerun of
                    [] -> finish damlTestResults
                    _ -> do
                      lspResults <- runTestLspSession damlLocation testFile logLsp functionsToRerun
                      case lspResults of
                        Left err -> logExitFailure $ "Tests failed to run: " <> err
                        Right lspResults' -> do
                          let
                            lspReport = report definedSteps lspResults' features
                            combineResult :: (Step, Text, StepReport) -> (Step, Text, StepReport) -> (Step, Text, StepReport)
                            combineResult (stp, fn, r) (_stp', _fn', r') = (stp, fn, case r' of
                                      StepDidNotRun -> r
                                      _ -> r')
                            combineReports :: Report -> Report -> Report
                            combineReports r1 r2 =
                              List.zipWith (\(f, xs) (_, ys) -> (f, zipWith (\(s, stps) (_, stps') -> (s, zipWith combineResult stps stps')) xs ys)) r1 r2
                            finalReport = formatOutlineSteps <$> combineReports damlTestResults lspReport
                          finish finalReport
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

testsSucceeded :: Report -> Bool
testsSucceeded r = all (==StepSucceeded) $ fmap (^._3)  $ squash $ squash r

testFailures :: Report -> Report
testFailures = filterReport (\case StepFailed _ -> True; _ -> False)

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
      , "  " <> summaryLine "Steps:     " totalSteps numFailingSteps
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
            , let csWithMatchingD = [(c, fn, d) | (c, fn, d) <- cs, f d]
            , not (null csWithMatchingD)
            ]
    , not (null bsWithMatchingD)]

data StepReport = StepSucceeded | StepFailed Text | StepDidNotRun
  deriving (Show, Eq)

-- | We aren't using a Map here to preserve the ordering
type Report = [(Feature, [(Either Scenario Outline, [(Step, Text, StepReport)])])]

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
    getScenarioResults :: Scenario -> [(Step, Text, StepReport)]
    getScenarioResults = getResults id id getScenarioFunctionName

    getOutlineResults :: Outline -> [[(Step, Text, StepReport)]]
    getOutlineResults outline = mconcat $
      ffor (_outline_examples outline) $ \ex -> do
        (headerRow, values) <- maybeToList $ List.uncons (_examples_table ex)
        ffor (zip [(1::Int)..] values) $ \(index, vals) ->
          let formatStep = foldr (.) id $ fmap (\(h, v) step -> step { _step_body = T.replace ("<"<>h<>">") v (_step_body step) }) $ zip headerRow vals
           in getResults formatStep (_outline_scenario . snd) (uncurry getOutlineRowFunctionName) (index, outline)

    getResults :: (Step -> Step) -> (a -> Scenario) -> (a -> Text) -> a -> [(Step, Text, StepReport)]
    getResults formatStep getScenario getFunctionName a =
      let
        Scenario _ steps = getScenario a
        fname = getFunctionName a
        errors = maybe "" snd $ Map.lookup fname testResults
        resultMap = scenarioResultMap a
        getResult = flip Map.lookup resultMap
      in
        flip map steps $ \stp -> (formatStep stp,fname,) $
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
      (("  Scenario: " <> _scenario_name (either id _outline_scenario s)) :) $ ffor stps $ \(stp, _, result) ->
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
                  Just (StepFunc file fname) -> do
                    modify $ addImport $ T.pack file
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
