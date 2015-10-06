------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Domain.Scenarios.Services.ExtraServices where

import Control.Monad

import Data.Maybe

import System.FilePath(takeBaseName)

import Ideas.Common.Library
import Ideas.Service.State

import Domain.Scenarios.Condition
import Domain.Scenarios.Globals
import Domain.Scenarios.Scenario
import Domain.Scenarios.ScenarioState(ScenarioState)
import Domain.Scenarios.ScoringFunction(calculateScore, calculateSubScores)
import Domain.Scenarios.Services.Types

-- FeedbackForm Service -------------------------------------------------------------------------------------

feedbackform :: [FilePath] -> State a -> [(ID, String)]
feedbackform fs fstate = map (getFeedbackFormResult state) (scenarioFeedbackForm scenario)
  where 
    state = fromMaybe (error "Cannot give feedback on exercise: casting failed.") $
        castFrom (exercise fstate) (stateTerm fstate) :: ScenarioState
    scenario = findScript "feedbackform" fs (exercise fstate)
    
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
scenariolist = map (getScenarioInfo . readBinaryScenario)

-- Scenarioinfo service: shows the info for a specific scenario (exercise)
scenarioinfo :: [FilePath] -> Exercise a -> ScenarioInfo
scenarioinfo fs ex = getScenarioInfo (findScript "scenarioinfo" fs ex)

getScenarioInfo :: Scenario -> ScenarioInfo
getScenarioInfo scenario@(Scenario metadata _ _) = ScenarioInfo
                (show (getId scenario))
                (scenarioName           metadata)
                (scenarioDescription    metadata)
                (scenarioDifficulty     metadata)
                (scenarioBannerImage    metadata)
                (scenarioCharacterImage metadata)
                (scenarioModel          metadata)
                (map describeParameter (scenarioParameters metadata))
                (scenarioLocation       metadata)
                (scenarioPet            metadata)
                (scenarioToggles        metadata)
  where 
    describeParameter param = ParameterInfo
        (parameterId          param)
        (parameterName        param)      
        (parameterDescription param)
        
                                            
-- Score Service --------------------------------------------------------------------------------------------
      
-- Type-customized result structure
-- Score extremes are returned in a list, 
-- because EncoderJSON merges a tuple into the main structure of the result
score :: [FilePath] -> State a -> ScoreResult
score fs fstate = ScoreResult mainScore subScores mainScoreExtremes
    where metaData = scenarioMetaData (findScript "score" fs (exercise fstate))
          state = fromMaybe (error "Cannot score exercise: casting failed.") $
            castFrom (exercise fstate) (stateTerm fstate) :: ScenarioState
          mainScore = calculateScore subScores (scenarioScoringFunction metaData) state
          subScores = calculateSubScores parameters state
          mainScoreExtremes = liftM (\(min, max) -> [min, max]) 
            (scenarioScoreExtremes metaData) :: Maybe [Score]          
          parameters = scenarioParameters metaData


-- | Finds the script of the exercise in the given filepaths list
findScript :: String -> [FilePath] -> Exercise a -> Scenario
findScript usage fs ex =
    case filter (\path -> "scenarios" # newId (takeBaseName path) == getId ex) fs of
            [path] -> readBinaryScenario path
            _             ->
                error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a scenario."
