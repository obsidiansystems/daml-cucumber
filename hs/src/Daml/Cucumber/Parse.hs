module Daml.Cucumber.Parse where

import Daml.Cucumber.Types
import qualified Language.Abacate as A
import Text.Parsec.Error

parseFeature :: FilePath -> IO (Either ParseError Feature)
parseFeature fp = do
  fmap fromAbacate <$> A.parseFile fp

fromAbacate :: A.Feature -> Feature
fromAbacate (A.Feature _ _ header _ elements _) = Feature
  { _feature_name = header
  , _feature_scenarios =
    [ fromAbacateScenario scenario 
    | A.FES (A.Scenario _ scenario) <- elements
    ]
  , _feature_outlines =
    [ Outline (fromExamples <$> examples) (fromAbacateScenario $ A.scBasicScenario scenario)
    | A.FESO (A.ScenarioOutline examples scenario) <- elements
    ]
  }
  where
    fromAbacateScenario s = Scenario (A.bsName s) (fromAbacateStep <$> A.bsSteps s)
    fromAbacateStep s = Step
      { _step_keyword = case A.stStepKeyword s of
          A.Given -> Given
          A.When -> When
          A.Then -> Then
          A.And -> And
          A.But -> But
      , _step_body = A.stBody s
      }
    fromExamples :: A.Examples -> Examples
    fromExamples (A.Examples _ name table) = Examples name table
