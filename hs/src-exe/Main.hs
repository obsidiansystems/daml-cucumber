module Main where

import Control.Concurrent
import Control.Monad
import Daml.Cucumber
import Daml.Cucumber.Types
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
  <*> strOption
      ( long "source"
      <> metavar "FILEPATH"
      <> help "Directory the daml.yaml points to your source" )

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

  -- withSystemTempFile "cucumber-input-json" $ \input inputHandle -> do
  --   hClose inputHandle -- why?
  --   withSystemTempFile "cucumber-output-json" $ \output outputHandle -> do
  --     hClose outputHandle -- why?
  --
  --     -- case ec of
  --     --   ExitFailure n -> putStrLn $ "Tests FAILED, unexpected exit, code " <> show n <> show feature
  --     --   ExitSuccess -> case result of
  --     --     Left unparseable -> putStrLn $ "Tests FAILED, unreadable results: " <> unparseable
  --     --     Right messages -> do
  --     --       putStrLn $ T.unpack feature
  --     --       forM_ messages $ \scenario -> do
  --     --         case scenario of
  --     --           (x:_) -> putStrLn $ "  Scenario: " <> T.unpack (_message_scenario x)
  --     --           _ -> pure ()
  --     --         forM_ scenario $ \step -> do
  --     --           putStr $ ("    " <>) $ case _message_step step of
  --     --             Nothing -> "<no step>"
  --     --             Just s -> show (_step_keyword s) <> " " <> T.unpack (_step_body s)
  --     --           putStrLn $ (" => " <>) $ case _message_result step of
  --     --             Nothing -> "Pending"
  --     --             Just (DamlEither_Left err) -> "Failed: " <> T.unpack err
  --     --             Just (DamlEither_Right ()) -> "Passed"
  --     --       pure ()
  --     outputInfo <- readFile output
  --     putStrLn outputInfo
