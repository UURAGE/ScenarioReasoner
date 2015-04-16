module Domain.Scenarios.Condition where

import Domain.Scenarios.ScriptState
import Domain.Scenarios.Globals(ID)

data Condition = And [Condition] -- ^ A list of conditions, all of which need to be satisfied 
               | Or [Condition] -- ^ A list of conditions, one of which needs to be satisfied
               | Condition ComparisonCondition -- ^ A comparison condition
               deriving (Show, Eq)

-- | A condition that compares the value of a parameter using a binary predicate
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
evaluateCondition :: Condition -> ScriptState -> Bool
evaluateCondition mainCondition state = evaluate mainCondition
    where evaluate :: Condition -> Bool
          evaluate condition = case condition of
            And subConditions    -> and . map evaluate $ subConditions
            Or  subConditions    -> or  . map evaluate $ subConditions
            Condition comparison -> evaluateComparisonCondition comparison state

-- | Evaluates the comparison based on the given state.
evaluateComparisonCondition :: ComparisonCondition -> ScriptState -> Bool
evaluateComparisonCondition comparison state = operator tested value
    where operator = getCompareOperator (conditionTest comparison)
          tested = getParamOrZero (conditionIdref comparison) state
          value  = conditionValue comparison

-- | Evaluates the possible condition based on the given state, if there is no condition evaluate to True
evaluateMaybeCondition :: Maybe Condition -> ScriptState -> Bool
evaluateMaybeCondition = maybe (const True) evaluateCondition

-- | Returns the binary predicate corresponding to the given operator type.
getCompareOperator :: CompareOperator -> (Int -> Int -> Bool)
getCompareOperator operator = case operator of
            LessThan           -> (<)
            LessThanEqualTo    -> (<=)
            EqualTo            -> (==)
            GreaterThanEqualTo -> (>=)
            GreaterThan        -> (>)
            NotEqualTo         -> (/=)