{-# LANGUAGE DeriveGeneric #-}
{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Expression where

import Control.Arrow
import Data.Binary
import Data.Maybe
import GHC.Generics

import Domain.Scenarios.Condition
import Domain.Scenarios.Globals
import Domain.Scenarios.ScenarioState
import qualified Domain.Scenarios.DomainData as DD

data Expression = Literal DD.Value
                | ParameterReference ID (Maybe ID)
                | Sum [Expression]
                | Scale Integer Integer Expression
                | Choose [(Condition, Expression)] Expression
    deriving (Show, Read, Eq, Generic)

instance Binary Expression

evaluateExpression :: ParameterState -> Expression -> DD.Value
evaluateExpression state expr = case expr of
    Literal val -> val
    ParameterReference idref characterIdref -> getParameterValue state idref characterIdref
    Sum exprs -> DD.VInteger (sum (map (fromDDInteger . evaluateExpression state) exprs))
    Scale scalar divisor subExpr -> DD.VInteger $ round
        (fromInteger (fromDDInteger (evaluateExpression state subExpr) * scalar) / fromInteger divisor :: Double)
    Choose cases defaultExpr -> evaluateExpression state $ fromMaybe defaultExpr
        (lookup True (map (first (evaluateCondition state)) cases))
