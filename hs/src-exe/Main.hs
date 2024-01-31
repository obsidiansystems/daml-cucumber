module Main where

import Daml.Cucumber
import Options.Applicative

opts :: Parser Opts
opts = Opts
  <$> strOption
      ( long "features"
      <> metavar "FILEPATH"
      <> help "Directory where feature files and daml files are" )
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
  <*> flag False True
      ( long "verbose"
      <> help "Show intermediate output from LSP test run" )

main :: IO ()
main = do
  options <- execParser $ info (opts <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Run cucumber tests in daml script"
    , header "daml-cucumber cli tool"
    ]
  runTestSuite options
