{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Domain.Scenarios.Services.Score where

import Data.Maybe

import Ideas.Common.Library
import Ideas.Service.Types
import Ideas.Service.State

import Domain.Scenarios.Parser
import Domain.Scenarios.Types(evaluateCondition, calculateScore, calculateSubScores, errorOnFail)

data ScoreResult = ScoreResult Score 
                               [SubScore] 
                               (Maybe [Score])
                               
type Score = Int
type SubScore = (String, String, Score)

tScoreResult :: Type a ScoreResult
tScoreResult =
    Iso ((<-!) pairify) (Pair (Tag "mainscore"          tInt)
                        (Pair (Tag "subscores"         (tList (tTuple3 tString tString tInt)))
                              (Tag "extremes"          (tMaybe (tList tInt)))))
        where pairify (ScoreResult score subscores extremes) = (score, (subscores, extremes))
        
-- Type-customized result structure
-- Score extremes are returned in a list, because EncoderJSON
-- merges a tuple into the main structure of the result
score :: [Script] -> State a -> ScoreResult
score scripts fstate = ScoreResult mainScore subScores mainScoreExtremes
    where script = findScript "score" scripts $ exercise fstate
          state = (fromMaybe (error "Cannot score exercise: casting failed.") $
                      castFrom (exercise fstate) (stateTerm fstate))
          mainScore = calculateScore
              (fromMaybe (error "Cannot score exercise: no scoring function found.") $
                  getScriptScoringFunction script)
              (state)
          subScores = calculateSubScores (fromMaybe [] $ getScriptParameters script) state
          mainScoreExtremes = (errorOnFail $ getScriptScoreExtremes script) >>= return . (\(min, max) -> [min, max])
          