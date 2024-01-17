-- |

module Daml.Cucumber.Daml.Parse where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Data.Foldable
import Daml.Cucumber.Daml.Tokenizer
import Daml.Cucumber.Daml.Parser

data Keyword = Given | When | Then | And | But
  deriving (Eq, Show, Enum, Bounded, Ord)

data Definition = Definition
  { definitionName :: Text
  , definitionStep :: Maybe (Keyword, Text)
  , definitionType :: TypeSig
  }
  deriving (Eq, Show)

data FunctionType = FunctionType
  { inputs :: [Type]
  , output :: Type
  }
  deriving (Eq, Show)

data Type = Type Text [Text]
  deriving (Eq, Show)

data TypeSig
  = Reg Type
  | Func FunctionType
  deriving (Eq, Show)

parser :: Parser [Definition]
parser = do
  mDef <- try parseDefinition
  case mDef of
    Just def -> (def:) <$> parser
    Nothing -> do
      eat
      ended <- isEof
      case ended of
        True -> pure []
        False -> parser

parseType :: Text -> Parser Type
parseType name = do
  n <- identifier
  when (n == name) $ fail ""
  rest <- idents name
  expect (== Arrow)
  pure $ Type n rest

idents :: Text -> Parser [Text]
idents thing = do
  ident <- peek
  case ident of
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
  accept (== BeginComment)
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
    _ -> pure []

parseStepBind :: Parser (Keyword, Text)
parseStepBind = do
  comment <- parseComment
  case comment of
    (Identifier ident : rest) -> do
      let
        parsedKeyword =
          case ident of
            "Given" -> Just Given
            "When" -> Just When
            "Then" -> Just Then
            "And" -> Just And
            "But" -> Just But
            _ -> Nothing
      case parsedKeyword of
        Just kw -> pure (kw, tokensToText rest)
        Nothing -> fail ""
    _ -> fail ""
  where
    tokensToText x = T.intercalate " " $ fmap tokenToText x

parseDefinition :: Parser Definition
parseDefinition = do
  mStepBind <- try parseStepBind
  ident <- identifier
  _ <- accept (==Colon)
  t <- parseTypeSig ident
  pure $ Definition ident mStepBind t

testParseAFile :: FilePath -> IO ()
testParseAFile path = do
  results <- parseFile path $ parseAll parseDefinition
  for_ results (putStrLn . show)
  pure ()
