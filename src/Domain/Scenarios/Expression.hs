{-# LANGUAGE DeriveGeneric #-}
{- © Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Expression where

import Control.Arrow
import Data.Binary
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics

import Domain.Scenarios.Condition
import Domain.Scenarios.Globals
import Domain.Scenarios.ScenarioState
import qualified Domain.Scenarios.DomainData as DD

data Expression = Literal DD.Value
                | ParameterReference ID (Maybe ID) Calculation
                | Sum [Expression]
                | Scale Integer Integer Expression
                | Choose [(Condition, Expression)] Expression
    deriving (Show, Read, Eq, Generic)

instance Binary Expression

data Calculation = CalculateValue | CalculatePercentage
    deriving (Show, Read, Eq, Generic)

instance Binary Calculation

evaluateExpression :: TypeMap -> ParameterState -> Expression -> DD.Value
evaluateExpression typeMap state expr = case expr of
    Literal val -> val
    ParameterReference idref characterIdref calc -> case calc of
        CalculateValue -> value
        CalculatePercentage -> DD.VInteger $ round
            (fromInteger (100 * (fromDDInteger value - imin)) / fromInteger (imax - imin) :: Double)
          where DD.TSimple (DD.TInteger (Just imin) (Just imax)) = M.findWithDefault unknownParameter idref typeMap
                unknownParameter = error ("Reference to unknown parameter " ++ idref)
      where value = getParameterValue state idref characterIdref
    Sum exprs -> DD.VInteger (sum (map (fromDDInteger . evaluateExpression typeMap state) exprs))
    Scale scalar divisor subExpr -> DD.VInteger $ round
        (fromInteger (fromDDInteger (evaluateExpression typeMap state subExpr) * scalar) / fromInteger divisor :: Double)
    Choose cases defaultExpr -> evaluateExpression typeMap state $ fromMaybe defaultExpr
        (lookup True (map (first (evaluateCondition typeMap state)) cases))
