module Domain.Scenarios.Services.Score where

import Control.Monad

import Data.Maybe

import System.FilePath(FilePath)

import Ideas.Common.Library
import Ideas.Service.State

import Domain.Scenarios.Parser(parseScenario, findScript)
import Domain.Scenarios.ScoringFunction(calculateScore, calculateSubScores)
import Domain.Scenarios.Globals(ID, Name, Score)
import Domain.Scenarios.Scenario

data ScoreResult = ScoreResult Score           -- Total score as a percentage
                               [SubScore]      -- All subscores for all parameters
                               (Maybe [Score]) -- Extremes of the total score

type SubScore = (ID, Name, Score) -- Score as a percentage
        
-- Type-customized result structure
-- Score extremes are returned in a list, because EncoderJSON
-- merges a tuple into the main structure of the result
score :: [FilePath] -> State a -> ScoreResult
score fs fstate = ScoreResult mainScore subScores mainScoreExtremes
    where metaData = scenarioMetaData (parseScenario (findScript "score" fs $ exercise fstate))
          state = fromMaybe (error "Cannot score exercise: casting failed.") $
                      castFrom (exercise fstate) (stateTerm fstate)
          mainScore = calculateScore parameters (scenarioScoringFunction metaData) state
          subScores = calculateSubScores parameters state
          mainScoreExtremes = liftM (\(min, max) -> [min, max]) (scenarioScoreExtremes metaData) :: Maybe [Score]
          
          parameters = scenarioParameters metaData
          