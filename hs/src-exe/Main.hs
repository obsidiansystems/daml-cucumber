module Main where

import Daml.Cucumber
import Options.Applicative

data Opts = Opts
  { _opts_directory :: FilePath
  , _opts_featureFile :: Maybe FilePath
  , _opts_damlSourceDir :: FilePath
  , _opts_allowMissing :: Bool
  , _opts_generateOnly :: Bool
  }

opts :: Parser Opts
opts = Opts
  <$> strOption
      ( long "directory"
      <> metavar "FILEPATH"
      <> help "Directory where feature files and daml files are" )
  <*> optional (strOption
      ( long "feature-file"
      <> metavar "FILEPATH"
      <> help "Directory where feature files and daml files are" ))
  <*> strOption
      ( long "source"
      <> metavar "FILEPATH"
      <> help "Directory the daml.yaml points to your source" )
  <*> flag False True
      ( long "allow-missing"
      <> help "Don't fail if steps are missing" )
  <*> flag False True
      ( long "generate-only"
      <> help "Generate daml test script but don't run the tests" )

main :: IO ()
main = do
  options <- execParser $ info (opts <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Run cucumber tests in daml script"
    , header "daml-cucumber cli tool"
    ]
  runTestSuite
    (_opts_directory options)
    (_opts_featureFile options)
    (_opts_damlSourceDir options)
    (_opts_allowMissing options)
    (_opts_generateOnly options)
