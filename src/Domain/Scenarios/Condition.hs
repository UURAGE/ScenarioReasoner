{-# LANGUAGE DeriveGeneric #-}
{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Condition where

import qualified Data.Map as M

import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Globals(ID)
import GHC.Generics
import Data.Binary

data Condition = And [Condition]               -- ^ A list of conditions, all of which need to be satisfied
               | Or [Condition]                -- ^ A list of conditions, one of which needs to be satisfied
               | Condition ComparisonCondition -- ^ A comparison condition
               deriving (Show, Eq, Read, Generic)

instance Binary Condition

-- | A condition that compares the value of a parameter by id to a value using a binary predicate
data ComparisonCondition = ComparisonCondition
        { conditionIdref :: ID
        , conditionTest  :: CompareOperator
        , conditionValue :: Int
        } deriving (Show, Eq, Read, Generic)

instance Binary ComparisonCondition

data CompareOperator = LessThan
                     | LessThanEqualTo
                     | EqualTo
                     | GreaterThanEqualTo
                     | GreaterThan
                     | NotEqualTo
                     deriving (Show, Eq, Read, Generic)

instance Binary CompareOperator

-- | Evaluates the possible condition based on the given state
evaluateMaybeCondition :: Maybe Condition -> ScenarioState -> Bool
evaluateMaybeCondition = maybe (const True) evaluateCondition

-- | Evaluates the condition based on the given state
evaluateCondition :: Condition -> ScenarioState -> Bool
evaluateCondition mainCondition state = evaluate mainCondition
    where evaluate :: Condition -> Bool
          evaluate condition = case condition of
            And    subConditions -> all evaluate subConditions
            Or     subConditions -> any evaluate subConditions
            Condition comparison -> evaluateComparisonCondition comparison state

-- | Evaluates the comparison based on the given state
evaluateComparisonCondition :: ComparisonCondition -> ScenarioState -> Bool
evaluateComparisonCondition comparison state = operator tested value
    where operator = getCompareOperator (conditionTest comparison)
          tested = getParameterValue state
          value  = conditionValue comparison

          getParameterValue (ScenarioState paramMap _) = M.findWithDefault 0 (conditionIdref comparison) paramMap

-- | Returns the binary predicate corresponding to the given operator type
getCompareOperator :: CompareOperator -> Int -> Int -> Bool
getCompareOperator operator = case operator of
            LessThan           -> (<)
            LessThanEqualTo    -> (<=)
            EqualTo            -> (==)
            GreaterThanEqualTo -> (>=)
            GreaterThan        -> (>)
            NotEqualTo         -> (/=)
