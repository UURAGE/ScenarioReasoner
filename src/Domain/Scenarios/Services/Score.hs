module Domain.Scenarios.Services.Score where

import Control.Monad

import Data.Maybe

import Ideas.Common.Library
import Ideas.Service.State

import Domain.Scenarios.Parser(parseScenario)
import Domain.Scenarios.ScoringFunction(calculateScore, calculateSubScores)
import Domain.Scenarios.Globals(ID, Name, Score, Script)
import Domain.Scenarios.Scenario
import Domain.Scenarios.Script(findScript)

data ScoreResult = ScoreResult Score           -- Total score
                               [SubScore]      -- All subscores for all parameters
                               (Maybe [Score]) -- Extremes of the total score

type SubScore = (ID, Name, Score)
        
-- Type-customized result structure
-- Score extremes are returned in a list, because EncoderJSON
-- merges a tuple into the main structure of the result
score :: [Script] -> State a -> ScoreResult
score scripts fstate = ScoreResult mainScore subScores mainScoreExtremes
    where metaData = scenarioMetaData (parseScenario (findScript "score" scripts $ exercise fstate))
          state = fromMaybe (error "Cannot score exercise: casting failed.") $
                      castFrom (exercise fstate) (stateTerm fstate)
          mainScore = calculateScore (scenarioScoringFunction metaData) state
          subScores = calculateSubScores (scenarioParameters metaData) state
          mainScoreExtremes = liftM (\(min, max) -> [min, max]) (scenarioScoreExtremes metaData)  --Maybe Monad