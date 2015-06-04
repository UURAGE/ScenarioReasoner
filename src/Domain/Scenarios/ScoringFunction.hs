module Domain.Scenarios.ScoringFunction where

import qualified Data.Map as M

import Domain.Scenarios.Globals
import Domain.Scenarios.Condition
import Domain.Scenarios.ScenarioState

-- | Datastructure for a function to calculate the score based on the current state
data ScoringFunction = Constant Score
                     | Sum [ScoringFunction]
                     | Scale Int ScoringFunction
                     | ParamRef ID
                     | IntegeredCondition Condition
    
instance Show ScoringFunction where
    show (Constant score)          = "Constant: "  ++ show score  ++ "\n"
    show (Sum sflist)              = "Sum: "       ++ show sflist ++ "\n"
    show (Scale scalar sf)         = "Scale: "     ++ show scalar ++ "\n\t" ++ show sf ++ "\n"
    show (ParamRef id)             = "ParamID: "   ++ show id     ++ "\n"
    show (IntegeredCondition cond) = "condition: " ++ show cond   ++ "\n"
                     
-- | Calculates the score based on the given state with a scoring function
calculateScore :: ScoringFunction -> ScenarioState -> Score
calculateScore mainScoringFunction state@(ScenarioState paramMap _ _) = calculate mainScoringFunction  
    where calculate scoringFunction = case scoringFunction of
            Constant           score        -> score
            Sum                subFunctions -> sum . map calculate $ subFunctions
            Scale     scalar   subFunction  -> scalar * calculate subFunction
            ParamRef           paramId      -> M.findWithDefault 0 paramId paramMap
            IntegeredCondition condition    -> if evaluateCondition condition state then 1 else 0

-- | Calculates the values of the scored parameters in the given state.
calculateSubScores :: [Parameter] -> ScenarioState -> [(ID, Name, Score)]
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
                                            | otherwise                 = round (toInteger shiftedValue / toInteger shiftedMax)
  where 
    shiftedMax   = shift maxValue minValue
    shiftedMin   = shift minValue minValue
    shiftedValue = shift value    minValue
    
clamp score _ _ = error "no maximum and minimum given"

shift :: ParameterValue -> ParameterValue -> ParameterValue
shift value minValue | minValue < 0 = value + (0 - minValue)
                     | otherwise    = value


