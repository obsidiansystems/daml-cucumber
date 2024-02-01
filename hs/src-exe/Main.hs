module Main where

import Daml.Cucumber
import Data.Version
import Options.Applicative
import Paths_daml_cucumber (version)
import Options.Applicative.NonEmpty

opts :: Parser Opts
opts = Opts
  <$> some1 (strOption
      ( long "features"
      <> short 'f'
      <> metavar "FILEPATH"
      <> help "Directory where cucumber feature files are located, or the path to a single feature file." ))
  <*> strOption
      ( long "source"
      <> metavar "FILEPATH"
      <> help "Directory containing daml.yaml" )
  <*> flag False True
      ( long "allow-missing"
      <> help "Don't fail if steps are missing" )
  <*> flag False True
      ( long "generate-only"
      <> help "Generate daml test script but don't run the tests" )
  <*> flag False True
      ( long "verbose"
      <> short 'v'
      <> help "Verbose output" )
  <*> flag False True
      ( long "log-lsp"
      <> help "Show intermediate output from LSP test run"
      <> internal )

main :: IO ()
main = do
  options <- execParser $ info (opts <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Run cucumber tests in daml script"
    , header $ "Welcome to daml-cucumber " <> showVersion version <> " by Obsidian Systems"
    ]
  start options
