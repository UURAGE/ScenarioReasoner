{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Domain.Scenarios.ScoringFunction where

import Domain.Scenarios.Globals
import Domain.Scenarios.Condition
import Domain.Scenarios.ScriptState

-- | Datastructure for a function to calculate the score based on the current state
data ScoringFunction = Constant Score
                     | Sum [ScoringFunction]
                     | Scale Int ScoringFunction
                     | ParamRef ID
                     | IntegeredCondition Condition
                     
-- | Calculates the value of a scoring function based on the given state.
calculateScore :: ScoringFunction -> ScriptState -> Score
calculateScore mainScoringFunction state = calculate mainScoringFunction  
    where calculate scoringFunction = case scoringFunction of
            Constant score               -> score
            Sum subFunctions             -> sum . map calculate $ subFunctions
            Scale scalar subFunction     -> scalar * calculate subFunction
            ParamRef paramId             -> getParamOrZero paramId state
            IntegeredCondition condition -> if evaluateCondition condition state then 1 else 0

-- | Calculates the values of the scored parameters in the given state.
calculateSubScores :: [Parameter] -> ScriptState -> [(ID, Name, Score)]
calculateSubScores parameters state = 
    map (\param -> ( parameterId param
                   , parameterName param
                   , getParamOrZero (parameterId param) state)
        ) . filter parameterScored $ parameters