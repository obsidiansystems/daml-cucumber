module Daml.Cucumber.Daml.Parser where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text.IO as T

import Daml.Cucumber.Daml.Tokenizer

newtype Parser a = Parser
  { unParser :: StateT [Token] Maybe a
  }
  deriving (Functor, Applicative, Monad, MonadFail, MonadState [Token])

runParser :: [Token] -> Parser a -> Maybe (a, [Token])
runParser tokens (Parser action) = runStateT action tokens

parseAll :: Parser a -> Parser [a]
parseAll action = do
  mDef <- try action
  case mDef of
    Just def -> (def:) <$> parseAll action
    Nothing -> do
      eat
      ended <- isEof
      case ended of
        True -> pure []
        False -> parseAll action

accept :: (Token -> Bool) -> Parser Token
accept f = do
  (x:_) <- get
  case f x of
    True -> do
      modify (drop 1)
      pure x
    False -> fail ""

expect :: (Token -> Bool) -> Parser ()
expect f = do
  tokens <- get
  case tokens of
    token:_ -> case f token of
      True -> modify (drop 1)
      False -> pure ()
    _ -> pure ()

identifier :: Parser Text
identifier = do
  Identifier name <- accept isIdentifier
  pure name

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile filePath action = do
  fileData <- T.readFile filePath
  let
    tokens = tokenize fileData
    result = runParser tokens action
  pure $ fmap fst result

isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier _ = False

isEof :: Parser Bool
isEof = gets null

peek :: Parser Token
peek = do
  (x:_) <- get
  pure x

eat :: Parser ()
eat = modify (drop 1)

try :: Parser a -> Parser (Maybe a)
try subAction = do
  tokens <- get
  let
    result = runParser tokens subAction
  case result of
    Just (p, rest) -> do
      put rest
      pure $ Just p
    Nothing -> pure Nothing
