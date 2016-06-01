{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Services.AdaptedServices where

import Data.List
import Data.Maybe

import Ideas.Common.Library
import Ideas.Service.BasicServices (StepInfo)
import Ideas.Service.State

import Domain.Scenarios.ScenarioState

-- This adaptation of allfirsts in Ideas.Service.BasicServices
-- merges duplicate rules with different states together,
-- when the strategy is not left factorised
-- and it returns an empty result if the scenario dialogue is at an end.
allfirsts :: State a -> Either String [(StepInfo a, State a)]
allfirsts state
   | withoutPrefix state = Left "Prefix is required"
   | otherwise = if end then Right [] else Right result
  where
    errorMessage = "Cannot get end of scenario from ScenarioState: casting failed."
    (ScenarioState _ _ end) = fromMaybe (error errorMessage)
        $ castFrom (exercise state) (stateTerm state) :: ScenarioState

    result = mergeDuplicates $ map make $ firsts state

    make ((r, ctx, env), st) =
      let pfx      = statePrefix st
          newState = (makeState (exercise state) pfx ctx)
                       { stateSession   = stateSession state
                       , stateUser      = stateUser state
                       , stateStartTerm = stateStartTerm state }
      in ((r, location ctx, env), newState)

    mergeDuplicates = mapMaybe mergeSteps . groupWith eq
    eq ((r1, _,_), _) ((r2, _, _), _) = getId r1 == getId r2

    groupWith :: (a -> a -> Bool) -> [a] -> [[a]]
    groupWith _ []     = []
    groupWith f (x:xs) =
       let (ys, zs) = partition (f x) xs
       in  (x:ys) : groupWith f zs

    mergeSteps :: [(StepInfo a, State a)] -> Maybe (StepInfo a, State a)
    mergeSteps xs = do
        (step, hState) <- safeHead xs
        return (step, hState { statePrefix = newPrefix })
      where
        newPrefix = mconcat [ statePrefix st | (_, st) <- xs ]

    safeHead :: [a] -> Maybe a
    safeHead (x:_) = Just x
    safeHead []    = Nothing
