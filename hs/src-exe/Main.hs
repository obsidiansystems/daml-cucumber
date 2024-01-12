module Main where

import Daml.Cucumber
import Options.Applicative
import System.IO.Temp

opts :: Parser Opts
opts = Opts
  <$> strOption
      ( long "dar"
      <> metavar "FILEPATH"
      <> help "Path to dar file" )
  <*> strOption
      ( long "host"
      <> metavar "HOST"
      <> help "Ledger sandbox host" )
  <*> intOption
      ( long "port"
      <> metavar "PORT"
      <> help "Ledger sandbox port" )
  <*> strOption
      ( long "feat"
      <> metavar "FILEPATH"
      <> help "Path to gherkin feature file" )
  <*> strOption
      ( long "script"
      <> metavar "MODULE:FUNCTION"
      <> help "Test script entry point (\"Module:run\")" )
  where
    intOption :: Mod OptionFields Int -> Parser Int
    intOption = option auto

main :: IO ()
main = do
  options <- execParser $ info (opts <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Run cucumber tests in daml script"
    , header "daml-cucumber cli tool"
    ]
  withSystemTempFile "cucumber-input-json" $ \input _ -> do
    withSystemTempFile "cucumber-output-json" $ \output _ -> do
      runCucumberFeature options input output
