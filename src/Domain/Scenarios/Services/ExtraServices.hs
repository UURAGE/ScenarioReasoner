module Domain.Scenarios.Services.ExtraServices where

import Control.Monad

import Data.Maybe

import System.FilePath(FilePath)

import Ideas.Common.Library
import Ideas.Service.State
import Ideas.Service.Types

import Domain.Scenarios.Condition
import Domain.Scenarios.Globals
import Domain.Scenarios.Parser
import Domain.Scenarios.Scenario
import Domain.Scenarios.ScenarioState(ScenarioState)
import Domain.Scenarios.ScoringFunction(SubScore, calculateScore, calculateSubScores)
import Domain.Scenarios.Services.Types

-- FeedbackForm Service -------------------------------------------------------------------------------------

feedbackform :: [FilePath] -> State a -> [(ID, String)]
feedbackform fs fstate = map (getFeedbackFormResult state) (scenarioFeedbackForm metaData)
  where 
    state = fromMaybe (error "Cannot give feedback on exercise: casting failed.") $
        castFrom (exercise fstate) (stateTerm fstate) :: ScenarioState
    metaData = scenarioMetaData (parseScenario (findScript "feedbackform" fs (exercise fstate)))
    
-- If an entry contains conditioned feedback then it checks if it is fullfilled 
-- and returns the corresponding feedback otherwise it returns the default feedback or an empty string.
getFeedbackFormResult :: ScenarioState -> FeedbackFormEntry -> (ID, String)
getFeedbackFormResult state (FeedbackFormEntry paramID feedbackConditions defaultFeedback) =
    case evaluateFBConditions feedbackConditions of
        Nothing       -> (paramID, fromMaybe "" defaultFeedback)
        Just feedback -> (paramID, feedback)
  where 
    evaluateFBConditions :: [(Condition, String)] -> Maybe String
    evaluateFBConditions []                                                = Nothing
    evaluateFBConditions ((cond, fb) : cfs) | evaluateCondition cond state = Just fb
                                            | otherwise                    = evaluateFBConditions cfs
                                            

-- ScenarioList and Info Service -------------------------------------------------------------------------------------
  
-- Scenariolist service: lists all info for each scenario
scenariolist :: [FilePath] -> [ScenarioInfo]
scenariolist = map (getScenarioInfo . parseScenario . parseScript)

-- Scenarioinfo service: shows the info for a specific scenario (exercise)
scenarioinfo :: [FilePath] -> Exercise a -> ScenarioInfo
scenarioinfo fs ex = getScenarioInfo (parseScenario (findScript "get info for" fs ex))

getScenarioInfo :: Scenario -> ScenarioInfo
getScenarioInfo scenario@(Scenario metadata _) = ScenarioInfo
                (show (getId scenario))
                (scenarioName           metadata)
                (scenarioDescription    metadata)
                (scenarioDifficulty     metadata)
                (scenarioBannerImage    metadata)
                (scenarioCharacterImage metadata)
                (scenarioModel          metadata)
                (map describeParameter (scenarioParameters metadata))
                (scenarioLocation       metadata)
                (scenarioToggles        metadata)
  where 
    describeParameter param = ParameterInfo
        (parameterId      param)
        (parameterName    param)      
        
                                            
-- Score Service --------------------------------------------------------------------------------------------
      
-- Type-customized result structure
-- Score extremes are returned in a list, 
-- because EncoderJSON merges a tuple into the main structure of the result
score :: [FilePath] -> State a -> ScoreResult
score fs fstate = ScoreResult mainScore subScores mainScoreExtremes
    where metaData = scenarioMetaData (parseScenario (findScript "score" fs $ exercise fstate))
          state = fromMaybe (error "Cannot score exercise: casting failed.") $
            castFrom (exercise fstate) (stateTerm fstate) :: ScenarioState
          mainScore = calculateScore subScores (scenarioScoringFunction metaData) state
          subScores = calculateSubScores parameters state
          mainScoreExtremes = liftM (\(min, max) -> [min, max]) 
            (scenarioScoreExtremes metaData) :: Maybe [Score]          
          parameters = scenarioParameters metaData
