module Daml.Cucumber.Daml.Parse
  ( DamlFile(..)
  , Definition(..)
  , Type(..)
  , TypeSig(..)
  , parseDamlFile
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Daml.Cucumber.Daml.Parser
import Daml.Cucumber.Daml.Tokenizer
import Daml.Cucumber.Types
import Data.Text (Text)
import qualified Data.Text as T

data DamlFile = DamlFile
  { damlFilePath :: FilePath
  , damlFileDefinitions :: [Definition]
  }

data Definition = Definition
  { definitionName :: Text
  , definitionStep :: Maybe Step
  , definitionType :: TypeSig
  }
  deriving (Eq, Show)

data FunctionType = FunctionType
  { inputs :: [Type]
  , output :: Type
  }
  deriving (Eq, Show)

data Type = Type Text [Text]
  deriving (Eq, Ord, Show)

data TypeSig
  = Reg Type
  | Func FunctionType
  deriving (Eq, Show)

parseType :: Text -> Parser Type
parseType name = do
  n <- identifier
  when (n == name) $ fail ""
  rest <- idents name
  expect (== Arrow)
  pure $ Type n rest

idents :: Text -> Parser [Text]
idents thing = peek >>= \case
  Identifier name | name /= thing -> do
    eat
    rest <- idents thing
    pure $ name : rest
  _ -> pure []

parseTypeSig :: Text -> Parser TypeSig
parseTypeSig name = do
  sig <- getSig
  case sig of
    [] -> fail ""
    (x:[]) -> pure $ Reg x
    other -> pure $ Func $ FunctionType (init other) (last other)
  where
    getSig = do
      result <- try (parseType name)
      case result of
        Just t -> (t :) <$> getSig
        Nothing -> pure []

parseComment :: Parser [Token]
parseComment = do
  _ <- accept (== BeginComment)
  tokens <- getTheComment
  pure tokens

getTheComment :: Parser [Token]
getTheComment = do
  token <- peek
  case token of
    LineBreak -> do
      eat
      pure []
    _ -> do
      eat
      (token :) <$> getTheComment

parseStepBinding :: Parser Step
parseStepBinding = do
  comment <- parseComment
  case comment of
    (Identifier ident' : rest) -> do
      let
        parsedKeyword =
          case ident' of
            "Given" -> Just Given
            "When" -> Just When
            "Then" -> Just Then
            "And" -> Just And
            "But" -> Just But
            _ -> Nothing
      case parsedKeyword of
        Just kw -> pure $ Step kw (tokensToText rest)
        Nothing -> fail ""
    _ -> fail ""
  where
    tokensToText x = T.intercalate " " $ fmap tokenToText x

parseDefinition :: Parser Definition
parseDefinition = do
  mStepBind <- try parseStepBinding
  ident' <- identifier
  _ <- accept (==Colon)
  t <- parseTypeSig ident'
  pure $ Definition ident' mStepBind t

parseFileDefinitions :: FilePath -> IO (Maybe [Definition])
parseFileDefinitions path = do
  parseFile path $ parseAll parseDefinition

parseDamlFile :: MonadIO m => FilePath -> m (Maybe DamlFile)
parseDamlFile path =
  fmap (DamlFile path) <$> liftIO (parseFileDefinitions path)
