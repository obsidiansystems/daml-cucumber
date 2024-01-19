{-# LANGUAGE ScopedTypeVariables #-}
module Daml.Cucumber.Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

data Keyword = Given | When | Then | And | But
  deriving (Eq, Show, Read, Enum, Bounded, Generic, Ord)

instance ToJSON Keyword
instance FromJSON Keyword

data StepKey = StepKey
  { keyword :: Keyword
  , identifier :: Text
  }
  deriving (Eq, Show, Ord, Generic)

instance ToJSON StepKey
instance FromJSON StepKey

data Message
  = StepComplete StepKey
  | ScenarioComplete Text
  | DuplicateStepFound StepKey
  | DuplicateScenarioFound Text
  deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "StepComplete" -> StepComplete <$> o .: "value"
      "ScenarioComplete" -> ScenarioComplete <$> o .: "value"
      "DuplicateStepFound" -> DuplicateStepFound <$> o .: "value"
      "DuplicateScenarioFound" -> DuplicateScenarioFound <$> o .: "value"
      _ -> fail $ "Invalid message tag " <> T.unpack tag


data Scenario = Scenario
  { _scenario_name :: Text
  , _scenario_steps :: [Step]
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Scenario where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (T.length "_scenario_")
  })

instance FromJSON Scenario where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (T.length "_scenario_")
  })

data Step = Step
  { _step_keyword :: Keyword
  , _step_body :: Text
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Step where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (T.length "_step_")
  })

instance FromJSON Step where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (T.length "_step_")
  })

data Outline = Outline
  { _outline_examples :: [Examples]
  , _outline_scenario :: Scenario
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Outline where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (T.length "_outline_")
  })

instance FromJSON Outline where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (T.length "_outline_")
  })

data Examples = Examples
  { _examples_name :: Text
  , _examples_table :: [[Text]]
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Examples where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (T.length "_examples_")
  })

instance FromJSON Examples where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (T.length "_examples_")
  })

data StepResult = StepResult
  { stepKeyword :: Keyword
  , ident :: Text
  , success :: Bool
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON StepResult
instance FromJSON StepResult

data Feature = Feature
  { _feature_name :: Text
  , _feature_scenarios :: [Scenario]
  , _feature_outlines :: [Outline]
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Feature where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (T.length "_feature_")
  })

instance FromJSON Feature where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (T.length "_feature_")
  })

data DamlEither a b = DamlEither_Left a | DamlEither_Right b
  deriving (Show, Read, Eq, Ord, Generic)

damlEitherJsonOpts :: Options
damlEitherJsonOpts = defaultOptions
  { sumEncoding = defaultTaggedObject
    { tagFieldName = "tag"
    , contentsFieldName = "value"
    }
  , constructorTagModifier = drop (T.length "DamlEither_")
  }

instance (ToJSON a, ToJSON b) => ToJSON (DamlEither a b) where
  toEncoding = genericToEncoding damlEitherJsonOpts

instance (FromJSON a, FromJSON b) => FromJSON (DamlEither a b) where
  parseJSON = genericParseJSON damlEitherJsonOpts

-- data Message = Message
--   { _message_scenario :: Text
--   , _message_step :: Maybe Step
--   , _message_result :: Maybe (DamlEither Text ())
--   }
--   deriving (Eq, Show, Read, Generic)
--
-- instance ToJSON Message where
--   toEncoding = genericToEncoding (defaultOptions {
--     fieldLabelModifier = drop (T.length "_message_")
--   })
--
-- instance FromJSON Message where
--   parseJSON = genericParseJSON (defaultOptions {
--     fieldLabelModifier = drop (T.length "_message_")
--   })
