module Domain.Scenarios.Condition where

import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Globals(ID)

data Condition = And [Condition]               -- ^ A list of conditions, all of which need to be satisfied 
               | Or [Condition]                -- ^ A list of conditions, one of which needs to be satisfied
               | Condition ComparisonCondition -- ^ A comparison condition
               deriving (Show, Eq)

-- | A condition that compares the value of a parameter by id to a value using a binary predicate
data ComparisonCondition = ComparisonCondition
        { conditionIdref :: ID
        , conditionTest  :: CompareOperator
        , conditionValue :: Int
        } deriving (Show, Eq)
        
data CompareOperator = LessThan
                     | LessThanEqualTo
                     | EqualTo
                     | GreaterThanEqualTo
                     | GreaterThan
                     | NotEqualTo
                     deriving (Show, Eq, Read)
                     
-- | Evaluates the condition based on the given state.
evaluateCondition :: Condition -> ScenarioState -> Bool
evaluateCondition mainCondition state = evaluate mainCondition
    where evaluate :: Condition -> Bool
          evaluate condition = case condition of
            And    subConditions -> all evaluate $ subConditions
            Or     subConditions -> any evaluate $ subConditions
            Condition comparison -> evaluateComparisonCondition comparison state

-- | Evaluates the comparison based on the given state.
evaluateComparisonCondition :: ComparisonCondition -> ScenarioState -> Bool
evaluateComparisonCondition comparison state = operator tested value
    where operator = getCompareOperator (conditionTest comparison)
          tested = getParameterValue state
          value  = conditionValue comparison
          
          getParameterValue (ScenarioState paramMap emotionMap _) | paramValue /= 0 = paramValue
                                                                  | otherwise       = emotionValue            
            where
              paramValue = getParamOrZero (conditionIdref comparison) paramMap
              emotionValue = getParamOrZero (conditionIdref comparison) emotionMap

-- | Evaluates the possible condition based on the given state, if there is no condition evaluate to True
evaluateMaybeCondition :: Maybe Condition -> ScenarioState -> Bool
evaluateMaybeCondition = maybe (const True) evaluateCondition

-- | Returns the binary predicate corresponding to the given operator type.
getCompareOperator :: CompareOperator -> Int -> Int -> Bool
getCompareOperator operator = case operator of
            LessThan           -> (<)
            LessThanEqualTo    -> (<=)
            EqualTo            -> (==)
            GreaterThanEqualTo -> (>=)
            GreaterThan        -> (>)
            NotEqualTo         -> (/=)