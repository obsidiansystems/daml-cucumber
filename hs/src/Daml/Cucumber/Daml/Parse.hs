module Daml.Cucumber.Daml.Parse
  ( DamlFile(..)
  , Definition(..)
  , Type(..)
  , TypeSig(..)
  , TypeSynonym(..)
  , parseDamlFile
  ) where

import Reflex (fmapMaybe)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Daml.Cucumber.Daml.Parser
import Daml.Cucumber.Daml.Tokenizer
import Daml.Cucumber.Types
import Data.Text (Text)
import qualified Data.Text as T

data DamlFile = DamlFile
  { damlFilePath :: FilePath
  , damlFileTypeSynonyms :: [TypeSynonym]
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

data TypeSynonym =
  TypeSynonym TypeSig TypeSig
  deriving (Eq, Show)

data Type = Type Text [Text]
  deriving (Eq, Ord, Show)

data TypeSig
  = Reg Type
  | Func FunctionType
  deriving (Eq, Show)

-- | A single element of DAML AST
data DamlNode
  = DamlDefinition Definition
  | DamlMultilineComment Text
  | DamlTypeSynonym TypeSynonym
  | DamlComment Text
  deriving (Eq, Show)

makePrisms ''DamlNode

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

parseMultiLine :: Parser Text
parseMultiLine = do
  _ <- accept (== BeginMultiLine)
  tokens <- takeUntil (== EndMultiLine)
  pure $ tokensToText tokens

parseTypeSynonym :: Parser TypeSynonym
parseTypeSynonym = do
  _ <- accept (== TypeKeyword)
  from <- parseTypeSig ""
  _ <- accept (== Equals)
  to <- parseTypeSig ""
  pure $ TypeSynonym from to

tokensToText :: [Token] -> Text
tokensToText = T.intercalate " " . fmap tokenToText

parseComment :: Parser [Token]
parseComment = do
  _ <- accept (== BeginComment)
  takeUntil (==LineBreak)

takeUntil :: (Token -> Bool) -> Parser [Token]
takeUntil f = do
  token <- peek
  case f token of
    True -> do
      eat
      pure []
    False -> do
      eat
      (token :) <$> takeUntil f

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

parseDamlNode :: Parser DamlNode
parseDamlNode =
  (DamlMultilineComment <$> parseMultiLine)
  <|> (DamlTypeSynonym <$> parseTypeSynonym)
  <|> (DamlDefinition <$> parseDefinition)
  <|> (DamlComment . tokensToText <$> parseComment)

parseDamlNodes :: MonadIO m => FilePath -> m (Maybe [DamlNode])
parseDamlNodes path = liftIO $ do
  parseFile path $ parseAll parseDamlNode

parseDamlFile :: MonadIO m => FilePath -> m (Maybe DamlFile)
parseDamlFile path = do
  nodes <- parseDamlNodes path
  let
    defs = nodes >>= pure . fmapMaybe (preview _DamlDefinition)
    typeSyns = nodes >>= pure . fmapMaybe (preview _DamlTypeSynonym)
  pure $ DamlFile path <$> typeSyns <*> defs
