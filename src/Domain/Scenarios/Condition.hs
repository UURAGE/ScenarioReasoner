{-# LANGUAGE DeriveGeneric #-}
{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Condition where

import Data.Binary
import GHC.Generics

import qualified Domain.Scenarios.DomainData as DD
import Domain.Scenarios.Globals(ID, ParameterState, getParameterValue)

data Condition = And [Condition]               -- ^ A list of conditions, all of which need to be satisfied
               | Or [Condition]                -- ^ A list of conditions, one of which needs to be satisfied
               | Condition ComparisonCondition -- ^ A comparison condition
               deriving (Show, Eq, Read, Generic)

instance Binary Condition

-- | A condition that compares the value of a parameter by id to a value using a binary predicate
data ComparisonCondition = ComparisonCondition
        { conditionIdref          :: ID
        , conditionCharacterIdref :: Maybe ID
        , conditionTest           :: CompareOperator
        , conditionValue          :: DD.Value
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

-- | Evaluates the condition based on the given state
evaluateCondition :: ParameterState -> Condition -> Bool
evaluateCondition state = evaluate
    where evaluate :: Condition -> Bool
          evaluate condition = case condition of
            And    subConditions -> all evaluate subConditions
            Or     subConditions -> any evaluate subConditions
            Condition comparison -> evaluateComparisonCondition state comparison

-- | Evaluates the comparison based on the given state
evaluateComparisonCondition :: ParameterState -> ComparisonCondition -> Bool
evaluateComparisonCondition state comparison = operator tested value
    where operator = getCompareOperator (conditionTest comparison)
          tested = getParameterValue state idref characterIdref
          value = conditionValue comparison
          idref = conditionIdref comparison
          characterIdref = conditionCharacterIdref comparison

-- | Returns the binary predicate corresponding to the given operator type
getCompareOperator :: CompareOperator -> DD.Value -> DD.Value -> Bool
getCompareOperator operator = case operator of
            LessThan           -> liftIntCompareOperator (<)
            LessThanEqualTo    -> liftIntCompareOperator (<=)
            EqualTo            -> (==)
            GreaterThanEqualTo -> liftIntCompareOperator (>=)
            GreaterThan        -> liftIntCompareOperator (>)
            NotEqualTo         -> (/=)
    where liftIntCompareOperator op (DD.VInteger i1) (DD.VInteger i2) = i1 `op` i2
          liftIntCompareOperator _ v1 v2 = error ("Cannot compare " ++ show v1 ++ " and " ++ show v2)
