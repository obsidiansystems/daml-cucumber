module Daml.Package.Generate where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AM
import Control.Monad.IO.Class
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable
import Development.Shake.FilePath (makeRelativeEx)
import System.FilePath
import System.Directory qualified as Dir
import System.Process

import Daml.Cucumber
import Daml.Cucumber.Daml.Parse
import Daml.Cucumber.Daml.Yaml qualified as Yaml

data PackageInfo = PackageInfo
  { _packageInfo_darLocation :: FilePath
  , _packageInfo_location :: FilePath
  , _packageInfo_symlinkLocation :: FilePath
  , _packageInfo_yamlWithoutLocalDeps :: Yaml.DamlYaml
  , _packageInfo_absoluteDeps :: [FilePath]
  }

createPackageHierarchy
  :: FilePath -- ^ path to where the daml.yaml of the project lives
  -> IO ()
createPackageHierarchy path = do
  -- Get the main daml.yaml
  let damlYamlPath = path </> "daml.yaml"
  damlYaml <- Yaml.parseDamlYaml damlYamlPath >>= \case
    Left err -> fail (show err)
    Right yml -> pure yml
  -- Get the daml sources
  let mainSourcePath = path </> Yaml.damlYaml_source damlYaml
  damlFiles <- liftIO (findDamlSources mainSourcePath)
  (failures, parsed) <- fmap partitionEithers $ forM damlFiles $ \f -> do
    parseDamlFile f >>= \case
      Nothing -> pure $ Left f
      Just df -> pure $ Right df
  let depGraph = AM.overlays $ flip fmap parsed $ \p ->
        let p' = p
              { damlFileImports = filter (\i -> not ("DA." `T.isPrefixOf` unImportedModule i || "Daml." `T.isPrefixOf` unImportedModule i)) (damlFileImports p)
              }
         in importEdges p'
      depMap = AM.adjacencyMap depGraph
      packageTreeRoot = path </> "package"
      -- refers to itself to resolve where the dar files live
      yamlMap = Map.fromList $ flip fmap parsed $ \p ->
        let dottedPackageName = moduleHeader_name (damlModuleHeader p)
            dashedPackageName = T.replace "." "-" dottedPackageName
            packageVersion = Yaml.damlYaml_version damlYaml
            packageLocation = packageTreeRoot </> T.unpack dottedPackageName
            packageSourceBase = packageLocation </> "daml"
            packageSymlinkLocation = addExtension (foldl (</>) packageSourceBase (fmap T.unpack $ T.split (== '.') dottedPackageName)) "daml"
            packageDarLocation = addExtension
              (path </> "lib" </> T.unpack dottedPackageName </> T.unpack packageVersion </> T.unpack (dashedPackageName <> "-" <> packageVersion))
              "dar"
            packageYamlDefinition = Yaml.DamlYaml
              { Yaml.damlYaml_sdkVersion = Yaml.damlYaml_sdkVersion damlYaml
              , Yaml.damlYaml_name = dashedPackageName
              , Yaml.damlYaml_source = "daml"
              , Yaml.damlYaml_version = packageVersion
              , Yaml.damlYaml_dependencies = Yaml.damlYaml_dependencies damlYaml
              }
         in (,) dottedPackageName $ PackageInfo
              { _packageInfo_darLocation = packageDarLocation
              , _packageInfo_yamlWithoutLocalDeps = packageYamlDefinition
              , _packageInfo_absoluteDeps = case Map.lookup dottedPackageName depMap of
                  Nothing -> []
                  -- knot tied here
                  Just ds -> flip fmap (Set.toList ds) $ \d -> case Map.lookup d yamlMap of
                    Nothing -> error $ "dependency is missing from parsed packages " <> show d
                    Just info -> _packageInfo_darLocation info
              , _packageInfo_location = packageLocation
              , _packageInfo_symlinkLocation = packageSymlinkLocation
              }
  case AM.topSort (AM.transpose depGraph) of
    Left err -> fail $ "package cycle detected!" <> " " <> show err
    Right sortedDeps -> do
      forM_ parsed $ \p -> do
        let packageName = moduleHeader_name (damlModuleHeader p)
        case Map.lookup packageName yamlMap of
          Nothing -> fail $ "Couldn't find parsed package info " <> show p
          Just packageInfo -> do
            let packageSourceDir = takeDirectory $ _packageInfo_symlinkLocation packageInfo
                packageLocation = _packageInfo_location packageInfo
                packageYamlLocation = packageLocation </> "daml.yaml"
                packageYamlDefinitionWithoutDeps = _packageInfo_yamlWithoutLocalDeps packageInfo
                packageSourceSymlinkLocation = _packageInfo_symlinkLocation packageInfo
            relativeDeps <- forM (_packageInfo_absoluteDeps packageInfo) $ \d -> do
              Just relD <- makeRelativeEx packageLocation d
              pure $ T.pack relD
            let packageYamlDefinition = packageYamlDefinitionWithoutDeps
                  { Yaml.damlYaml_dependencies = Yaml.damlYaml_dependencies packageYamlDefinitionWithoutDeps <> relativeDeps
                  }
            Just packageSourceSymlinkTarget <- makeRelativeEx packageSourceDir (damlFilePath p)
            Dir.createDirectoryIfMissing True packageSourceDir
            BS.writeFile packageYamlLocation $ Yaml.serializeDamlYaml packageYamlDefinition
            Dir.doesPathExist packageSourceSymlinkLocation >>= \case
              True -> Dir.removeFile packageSourceSymlinkLocation
              False -> pure ()
            Dir.createFileLink packageSourceSymlinkTarget packageSourceSymlinkLocation
      LBS.writeFile (packageTreeRoot </> "build-order.json") $ Aeson.encode sortedDeps
  pure ()

buildPackageHierarchy
  :: FilePath
  -> IO ()
buildPackageHierarchy path = do
  let packageTreeRoot = path </> "package"
      buildOrderFile = packageTreeRoot </> "build-order.json"
  fmap Aeson.eitherDecode (LBS.readFile buildOrderFile) >>= \case
    Left err -> fail $ show err
    Right (ps :: [Text]) -> forM_ ps $ \p -> do
      let packageRoot = packageTreeRoot </> T.unpack p
      damlYaml <- Yaml.parseDamlYaml (packageRoot </> "daml.yaml") >>= \case
        Left err -> fail (show err)
        Right yml -> pure yml
      let packageDarLocation = addExtension
            (path </> "lib" </> T.unpack p </> T.unpack (Yaml.damlYaml_version damlYaml) </> T.unpack (T.replace "." "-" p <> "-" <> Yaml.damlYaml_version damlYaml))
            "dar"
      print damlYaml
      (_,_,_,damlProc) <- createProcess $ (proc "daml" ["build", "-o", packageDarLocation]){
        env = Just [("DAML_PROJECT", packageRoot)]
      }
      waitForProcess damlProc


importEdges :: DamlFile -> AdjacencyMap Text
importEdges f = AM.vertex (moduleHeader_name (damlModuleHeader f)) `AM.connect` (AM.vertices $ fmap unImportedModule (damlFileImports f))


