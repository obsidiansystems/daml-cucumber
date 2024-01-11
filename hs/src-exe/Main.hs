module Main where

import Options.Applicative

data Opts = Opts
  { _opts_darPath :: FilePath
  , _opts_ledgerHost :: String
  , _opts_ledgerPort :: Int
  , _opts_featureFile :: FilePath
  }

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
  pure ()
  
