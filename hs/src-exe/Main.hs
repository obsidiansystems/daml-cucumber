module Main where

import Control.Concurrent
import Control.Monad
import Daml.Cucumber
import Daml.Cucumber.Types
import Daml.Cucumber.LSP
import qualified Data.Text as T
import Options.Applicative
import System.Exit
import System.IO
import System.IO.Temp

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
  runTestSuite options
