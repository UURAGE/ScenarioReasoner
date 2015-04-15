module Domain.Scenarios.Services.Score where

import Data.Maybe

import Ideas.Common.Library
import Ideas.Service.State

import Domain.Scenarios.Parser
import Domain.Scenarios.ScoringFunction(calculateScore, calculateSubScores)
import Domain.Scenarios.Globals(ID, Name, Score, ScriptElement)
import Domain.Scenarios.Id(findScript)

data ScoreResult = ScoreResult Score 
                               [SubScore] 
                               (Maybe [Score])

type SubScore = (ID, Name, Score)
        
-- Type-customized result structure
-- Score extremes are returned in a list, because EncoderJSON
-- merges a tuple into the main structure of the result
score :: [ScriptElement] -> State a -> ScoreResult
score scripts fstate = ScoreResult mainScore subScores mainScoreExtremes
    where metaData = parseMetaData (findScript "score" scripts $ exercise fstate)
          state = (fromMaybe (error "Cannot score exercise: casting failed.") $
                      castFrom (exercise fstate) (stateTerm fstate))
          mainScore = calculateScore (scriptScoringFunction metaData) state
          subScores = calculateSubScores (scriptParameters metaData) state
          mainScoreExtremes = scriptScoreExtremes metaData >>= return . (\(min, max) -> [min, max]) --Maybe Monad