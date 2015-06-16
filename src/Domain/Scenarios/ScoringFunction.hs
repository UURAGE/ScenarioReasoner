{-# LANGUAGE FlexibleContexts #-}
module Domain.Scenarios.ScoringFunction where

import Data.List(find)
import Data.Maybe(fromJust)
import qualified Data.Map as M

import Domain.Scenarios.Globals
import Domain.Scenarios.Condition
import Domain.Scenarios.ScenarioState

-- | Datastructure for a function to calculate the score based on the current state
data ScoringFunction = Constant Score
                     | Sum [ScoringFunction]
                     | Scale Int ScoringFunction
                     | ParamRef ID
    
instance Show ScoringFunction where
    show (Constant score)          = "Constant: "  ++ show score  ++ "\n"
    show (Sum sflist)              = "Sum: "       ++ show sflist ++ "\n"
    show (Scale scalar sf)         = "Scale: "     ++ show scalar ++ "\n\t" ++ show sf ++ "\n"
    show (ParamRef id)             = "ParamID: "   ++ show id     ++ "\n"
    
type SubScore = (ID, Name, Score) -- Score as a percentage
                     
-- | Calculates the score based on the given state with a scoring function
calculateScore :: [SubScore] -> ScoringFunction -> ScenarioState -> Score
calculateScore subScores mainScoringFunction state@(ScenarioState paramMap _ _) =
    (\(mainScore, weight) -> round (mainScore `divInt` weight)) (calculate mainScoringFunction 0)
  where 
    calculate :: ScoringFunction -> Int -> (Score, Int)
    calculate scoringFunction weight = case scoringFunction of
        Constant     score        -> (score, weight + 1)
        Sum          subFunctions -> sumScoreAndWeight (map (flip calculate $ weight) subFunctions)
        Scale scalar subFunction  -> (\(subScore, _) -> (scalar * subScore, weight + scalar)) (calculate subFunction scalar)
        ParamRef     paramId      | weight == 0 -> (0, 0)
        ParamRef     paramId      | otherwise   -> (findSubScore paramId, weight)
    
    findSubScore :: ID -> Score
    findSubScore paramId = trd3 (fromJust (find ((==) paramId . fst3) subScores))
    
    sumScoreAndWeight :: [(Score, Int)] -> (Score, Int)
    sumScoreAndWeight sws = (sum $ map fst sws, sum $ map snd sws)
    
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x
    
    trd3 :: (a, b, c) -> c 
    trd3 (_, _, z) = z

-- | Calculates the values of the scored parameters in the given state.
calculateSubScores :: [Parameter] -> ScenarioState -> [SubScore]
calculateSubScores parameters (ScenarioState paramMap _ _) = 
    map (\param -> ( parameterId     param
                   , parameterName   param
                   , clamp (getParamValue param) (parameterMax param) (parameterMin param))
        ) . filter parameterScored $ parameters
        
  where 
    getParamValue param = M.findWithDefault 0 (parameterId param) paramMap
  
clamp :: ParameterValue -> Maybe ParameterValue -> Maybe ParameterValue -> Score
clamp value (Just maxValue) (Just minValue) | shiftedValue > shiftedMax = 100
                                            | shiftedValue < shiftedMin = 0
                                            | otherwise                 = round ((shiftedValue `divInt` shiftedMax) * 100)
  where 
    shiftedMax   = shift maxValue minValue
    shiftedMin   = shift minValue minValue
    shiftedValue = shift value    minValue
    
clamp score _ _ = error "no maximum and minimum given"

shift :: ParameterValue -> ParameterValue -> ParameterValue
shift value minValue | minValue < 0 = value + (0 - minValue)
                     | otherwise    = value

divInt :: (Fractional a) => Int -> Int -> a
divInt numerator denominator = fromIntegral numerator / fromIntegral denominator
