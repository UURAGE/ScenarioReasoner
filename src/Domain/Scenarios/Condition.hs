{-# LANGUAGE DeriveGeneric #-}
{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Condition where

import qualified Data.Map as M
import Data.Binary
import GHC.Generics

import qualified Domain.Scenarios.DomainData as DD
import Domain.Scenarios.Globals(ID, Usered(..), Charactered(..), ParameterState)
import Domain.Scenarios.ScenarioState

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

-- | Evaluates the possible condition based on the given state
evaluateMaybeCondition :: Maybe Condition -> ScenarioState -> Bool
evaluateMaybeCondition mCondition (ScenarioState parameterState _ _) =
    maybe True (`evaluateCondition` parameterState) mCondition

-- | Evaluates the condition based on the given state
evaluateCondition :: Condition -> ParameterState -> Bool
evaluateCondition mainCondition state = evaluate mainCondition
    where evaluate :: Condition -> Bool
          evaluate condition = case condition of
            And    subConditions -> all evaluate subConditions
            Or     subConditions -> any evaluate subConditions
            Condition comparison -> evaluateComparisonCondition comparison state

-- | Evaluates the comparison based on the given state
evaluateComparisonCondition :: ComparisonCondition -> ParameterState -> Bool
evaluateComparisonCondition comparison state = operator tested value
    where operator = getCompareOperator (conditionTest comparison)
          tested = getParameterValue state idref characterIdref
          value = conditionValue comparison
          idref = conditionIdref comparison
          characterIdref = conditionCharacterIdref comparison

-- | Retrieves the value of a parameter from the given state
getParameterValue :: ParameterState -> ID -> Maybe ID -> DD.Value
getParameterValue paramState idref mCharacterIdref = case mCharacterIdref of
        Just characterIdref -> M.findWithDefault unknownParameter idref $
                M.findWithDefault unknownCharacter characterIdref $ M.union
                    (characteredPerCharacter (useredUserDefined paramState))
                    (characteredPerCharacter (useredFixed paramState))
            where unknownCharacter = error ("Condition for unknown character " ++ characterIdref)
        Nothing -> M.findWithDefault unknownParameter idref $ M.union
            (characteredIndependent (useredUserDefined paramState))
            (characteredIndependent (useredFixed paramState))
    where unknownParameter = error ("Condition for unknown parameter " ++ idref)

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
