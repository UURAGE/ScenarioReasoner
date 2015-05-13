module Domain.Scenarios.ScoringFunction where

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
            ParamRef           paramId      -> getParamOrZero paramId paramMap
            IntegeredCondition condition    -> if evaluateCondition condition state then 1 else 0

-- | Calculates the values of the scored parameters in the given state.
calculateSubScores :: [Parameter] -> ScenarioState -> [(ID, Name, Score)]
calculateSubScores parameters (ScenarioState paramMap _ _) = 
    map (\param -> ( parameterId     param
                   , parameterName   param
                   , getParamOrZero (parameterId param) paramMap)
        ) . filter parameterScored $ parameters