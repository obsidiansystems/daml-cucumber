module Daml.Cucumber.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data Keyword = Given | When | Then | And | But
  deriving (Eq, Show, Read, Enum, Bounded, Generic)

instance ToJSON Keyword
instance FromJSON Keyword

data Scenario = Scenario
  { _scenario_name :: Text
  , _scenario_steps :: [Step]
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Scenario where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (length "_scenario_")
  })

instance FromJSON Scenario where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (length "_scenario_")
  })

data Step = Step
  { _step_keyword :: Keyword
  , _step_body :: Text
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Step where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (length "_step_")
  })

instance FromJSON Step where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (length "_step_")
  })

data Outline = Outline
  { _outline_examples :: [Examples]
  , _outline_scenario :: Scenario
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Outline where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (length "_outline_")
  })

instance FromJSON Outline where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (length "_outline_")
  })

data Examples = Examples
  { _examples_name :: Text
  , _examples_table :: [[Text]]
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Examples where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (length "_examples_")
  })

instance FromJSON Examples where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (length "_examples_")
  })

data Feature = Feature
  { _feature_name :: Text
  , _feature_scenarios :: [Scenario]
  , _feature_outlines :: [Outline]
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Feature where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (length "_feature_")
  })

instance FromJSON Feature where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (length "_feature_")
  })

data Message = Message
  { _message_scenario :: Text
  , _message_step :: Maybe Step
  , _message_result :: Maybe (Either Text ())
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Message where
  toEncoding = genericToEncoding (defaultOptions {
    fieldLabelModifier = drop (length "_message_")
  })

instance FromJSON Message where
  parseJSON = genericParseJSON (defaultOptions {
    fieldLabelModifier = drop (length "_message_")
  })


