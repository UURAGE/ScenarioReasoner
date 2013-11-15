module Domain.Scenarios.Services where

import Data.Maybe

import Ideas.Common.Library
import Ideas.Service.Types
import Ideas.Service.State

import Domain.Scenarios.Parser

customServices :: [Script] -> [Service]
customServices scripts = [alldescriptionsS, scoreS scripts]

alldescriptionsS :: Service
alldescriptionsS = makeService "scenarios.alldescriptions"
    "Returns the descriptions of all rules in the exercise." $
    alldescriptions ::: typed

alldescriptions :: Exercise a -> [(String, String)]
alldescriptions = map idAndDescription . ruleset
    where idAndDescription rule = (showId rule, description rule)

scoreS :: [Script] -> Service
scoreS scripts = makeService "scenarios.score"
    "Calculates the score of a given state." $
    (score scripts) ::: typed

score :: [Script] -> State a -> Int
score scripts state = case filter (\script -> (getId script) == (getId $ exercise state)) scripts of
    [script] ->
        calculateScore script $ fromMaybe (error "Cannot score exercise: casting failed.") $
            castFrom (exercise state) (stateTerm state)
    _        ->
        error "Cannot score exercise: exercise not found in list of scoreable exercises."