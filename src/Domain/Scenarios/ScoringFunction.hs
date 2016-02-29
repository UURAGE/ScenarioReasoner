{-# LANGUAGE DeriveGeneric #-}
------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Domain.Scenarios.ScoringFunction where

import Data.List(find)
import Data.Maybe(fromJust)
import qualified Data.Map as M

import Domain.Scenarios.Globals
import Domain.Scenarios.ScenarioState
import Data.Binary
import GHC.Generics

-- | Datastructure for a function to calculate the score based on the current state
data ScoringFunction = Constant Score
                     | Sum [ScoringFunction]
                     | Scale Int ScoringFunction
                     | ParamRef ID
 deriving (Show, Read,Generic)
 
instance Binary ScoringFunction

type SubScore = (ID, Name, Score) -- Score as a percentage

-- | Calculates the score based on the given state with a scoring function
calculateScore :: [SubScore] -> ScoringFunction -> ScenarioState -> Score
calculateScore subScores mainScoringFunction _ =
    (\(mainScore, weight) -> round (mainScore `divInt` weight)) (calculate mainScoringFunction 0)
  where 
    calculate :: ScoringFunction -> Int -> (Score, Int)
    calculate scoringFunction weight = case scoringFunction of
        Constant     score        -> (score, weight + 1)
        Sum          subFunctions -> sumScoreAndWeight (map (flip calculate $ weight) subFunctions)
        Scale scalar subFunction  -> (\(subScore, _) -> (scalar * subScore, weight + scalar)) (calculate subFunction scalar)
        ParamRef     _            | weight == 0 -> (0, 0)
        ParamRef     paramId      | otherwise   -> (findSubScore paramId, weight)
    
    -- | Finds the score for the given parameter
    findSubScore :: ID -> Score
    findSubScore paramId = trd3 (fromJust (find ((==) paramId . fst3) subScores))
    
    sumScoreAndWeight :: [(Score, Int)] -> (Score, Int)
    sumScoreAndWeight sws = (sum $ map fst sws, sum $ map snd sws)
    
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x
    
    trd3 :: (a, b, c) -> c 
    trd3 (_, _, z) = z

-- | Calculates the values of the scored parameters given a state
calculateSubScores :: [Parameter] -> ScenarioState -> [SubScore]
calculateSubScores parameters (ScenarioState paramMap _ _) = 
    map (\param -> ( parameterId     param
                   , parameterName   param
                   , clamp (getParamValue param) (parameterMax param) (parameterMin param))
        ) . filter parameterScored $ parameters
        
  where 
    getParamValue param = M.findWithDefault 0 (parameterId param) paramMap
  
-- Clamps the values between 0 and 100%
clamp :: ParameterValue -> Maybe ParameterValue -> Maybe ParameterValue -> Score
clamp value (Just maxValue) (Just minValue) | shiftedValue > shiftedMax = 100
                                            | shiftedValue < shiftedMin = 0
                                            | otherwise                 = round ((shiftedValue `divInt` shiftedMax) * 100)
  where 
    shiftedMax   = shift maxValue minValue
    shiftedMin   = shift minValue minValue
    shiftedValue = shift value    minValue
    
clamp _ _ _ = error "no maximum and minimum given"

-- Shifts the possible negative values to positive values given that maxValue >= minValue
shift :: ParameterValue -> ParameterValue -> ParameterValue
shift value minValue | minValue < 0 = value + (0 - minValue)
                     | otherwise    = value

-- Divides two integers and returns the result. If dividing by zero (weight) then return 0
divInt :: Int -> Int -> Double
divInt _         0           = 0
divInt numerator denominator = fromIntegral numerator / fromIntegral denominator
