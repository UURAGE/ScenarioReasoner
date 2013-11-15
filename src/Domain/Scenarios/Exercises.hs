module Domain.Scenarios.Exercises where

import Control.Monad

import Domain.Scenarios.State
import Domain.Scenarios.Parser
import Domain.Scenarios.Strategy

import Ideas.Common.Library

getExercises :: IO ([Exercise State], [Script])
getExercises = liftM unzip $ mapM getExercise [""]

getExercise :: String -> IO (Exercise State, Script)
getExercise path = do
    script <- parseScript path
    exercise <- exerciseFromScript script
    return (exercise, script)

exerciseFromScript :: Monad m => Script -> m (Exercise State)
exerciseFromScript script = do
    scriptDifficulty <- getScriptDifficulty script
    scriptStrategy <- makeStrategy script
    return makeExercise
       { exerciseId     = getId script
       , status         = Alpha
       , parser         = readJSON
       , prettyPrinter  = showJSON
       , equivalence    = (\_ _-> True)
       , similarity     = (\_ _-> True)
       , ready          = true
       , suitable       = true
       , hasTypeable    = useTypeable
       -- , extraRules     = undefined
       , strategy       = liftToContext $ label "The One And Only Strategy" scriptStrategy
       -- , navigation     = undefined
       , testGenerator  = Nothing
       -- , randomExercise = undefined
       , examples = [(scriptDifficulty, emptyState)]
       }