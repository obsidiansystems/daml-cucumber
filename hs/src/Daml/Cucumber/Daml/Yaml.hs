module Daml.Cucumber.Daml.Yaml where

import Control.Monad.IO.Class
import Data.Aeson.KeyMap qualified as Aeson
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml

data DamlYaml = DamlYaml
  { damlYaml_sdkVersion :: Text
  , damlYaml_name :: Text
  , damlYaml_source :: FilePath
  , damlYaml_version :: Text
  }
  deriving (Eq, Show)

data DamlYamlError
  = DamlYamlError_Parse ParseException
  | DamlYamlError_MissingField

parseDamlYaml :: MonadIO m => FilePath -> m (Either DamlYamlError DamlYaml)
parseDamlYaml fp = do
  yaml <- liftIO $ BS.readFile fp
  pure $ fromYamlParse $ decodeEither' yaml


fromYamlParse :: Either ParseException Value -> Either DamlYamlError DamlYaml
fromYamlParse = \case
    Right (Object v) ->
      let x = do
            source <- lookupString "source" v
            name <- lookupString "name" v
            sdkVersion <- lookupString "sdk-version" v
            version <- lookupString "version" v
            pure $ DamlYaml sdkVersion name (T.unpack source) version
      in case x of
            Nothing -> Left DamlYamlError_MissingField
            Just x' -> Right x'
    Right _ -> Left (DamlYamlError_Parse $ AesonException "Expected an object")
    Left a -> Left (DamlYamlError_Parse a)
  where
    lookupString k v = case Aeson.lookup k v of
      Just (String s) -> Just s
      _ -> Nothing

friendlyDamlYamlError :: DamlYamlError -> String
friendlyDamlYamlError = \case
  DamlYamlError_MissingField -> "daml.yaml is missing fields. It must have sdk-version, name, version, and source."
  DamlYamlError_Parse parseError -> prettyPrintParseException parseError

