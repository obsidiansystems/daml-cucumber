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
import System.FilePath hiding (hasExtension)
import Control.Monad.State

data Opts = Opts
  { _opts_directory :: FilePath
  , _opts_featureFile :: Maybe FilePath
  , _opts_damlSourceDir :: FilePath
  }

writeFeatureJson :: FilePath -> Feature -> IO ()
writeFeatureJson path f = LBS.writeFile path (encode f)

daml :: FilePath
daml = $(staticWhich "daml")

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
        (_, result) = runState (generateDamlSource stepMapping features) (DamlState mempty mempty)
      putStrLn $ show result
      writeDamlSource (damlFolder </> "Generated.daml") result
      pure ()
  pure ()

data StepFunc = StepFunc
  { stepFile :: FilePath
  , stepFunctionName :: Text
  }
  deriving (Eq, Show)

data DamlState = DamlState
  { damlImports :: Set Text
  , damlFunctions :: [DamlFunc]
  }
  deriving (Eq, Show)

data DamlFunc = DamlFunc
  { damlFuncName :: Text
  , damlFuncBody :: [Text]
  }
  deriving (Eq, Show)

addImport :: Text -> DamlState -> DamlState
addImport theImport state =
  state { damlImports = Set.insert theImport (damlImports state) }

addFunction :: DamlFunc -> DamlState -> DamlState
addFunction func state =
  state { damlFunctions = func : (damlFunctions state) }

generateDamlSource :: Map Step StepFunc -> [Feature] -> State DamlState ()
generateDamlSource stepMapping features = do
  for_ features $ \feature -> do
    for_ (_feature_scenarios feature) $ \scenario -> do
      fnames <- for (_scenario_steps scenario) $ \step -> do
        let
          Just (StepFunc file fname) = Map.lookup step stepMapping
        modify $ addImport $ T.pack file
        pure fname

      modify $ addFunction $ DamlFunc (T.pack $ toCamel $ fromWords $ T.unpack $ _scenario_name scenario) fnames
  pure ()

writeDamlSource :: FilePath -> DamlState -> IO ()
writeDamlSource path state = do
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

testSuiteContainsStep :: [Definition] -> Step ->  Bool
testSuiteContainsStep defns step = any ((Just step ==) . definitionStep) defns

testSuiteSupportsScenario :: [Definition] -> Scenario -> Bool
testSuiteSupportsScenario defns scenario =
  all (testSuiteContainsStep defns) (_scenario_steps scenario)

testSuiteSupportsFeature :: [Definition] -> Feature -> Bool
testSuiteSupportsFeature defns feature =
  all (testSuiteSupportsScenario defns) (_feature_scenarios feature)

-- TODO Barely scratches the surface of collation
data TestResults = TestResults
  { stepResuls :: Set StepKey
  , reportedErrors :: [Text]
  }

stepPresent :: StepKey -> TestResults -> Bool
stepPresent key (TestResults r _) = Set.member key r

instance Semigroup TestResults where
  (TestResults a b) <> (TestResults c d) = TestResults (a <> c) (b <> d)
instance Monoid TestResults where
  mempty = TestResults mempty mempty

collateResults :: [Message] -> TestResults
collateResults = mconcat . fmap toSet
  where
    toSet = \case
      StepComplete key -> TestResults (Set.singleton key) []
      DuplicateStepFound (StepKey keyword ident) -> TestResults mempty ["Step: " <> tShow keyword <> " " <> ident <> " is duplicated"]
      DuplicateScenarioFound name -> TestResults mempty ["Scenario:  " <> name <> " is duplicated"]
      _ -> mempty

-- END TODO
tShow :: Show a => a -> Text
tShow = T.pack . show

makeResultMap :: [StepResult] -> Map (Keyword, Text) StepResult
makeResultMap results =
  mconcat $ (\a@(StepResult k t _) -> Map.singleton (k,t) a) <$>  results
