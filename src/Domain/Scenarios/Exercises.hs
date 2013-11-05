module Domain.Scenarios.Exercises where

import Domain.Scenarios.State
import Domain.Scenarios.Parser

import Ideas.Common.Library

getExercises :: IO [Exercise State]
getExercises = mapM getExercise [""]

getExercise :: String -> IO (Exercise State)
getExercise path = parseScript path >>= exerciseFromScript

exerciseFromScript :: Monad m => Script -> m (Exercise State)
exerciseFromScript script = do
    scriptId <- getScriptId script
    scriptDescription <- getScriptDescription script
    scriptDifficulty <- getScriptDifficulty script
    return makeExercise
       { exerciseId     = describe scriptDescription $
                             newId ("scenarios." ++ scriptId)
       , status         = Alpha
       , parser         = readJSON
       , prettyPrinter  = showJSON
       , equivalence    = (\_ _-> True)
       , similarity     = (\_ _-> True)
       , ready          = true
       , suitable       = true
       -- , extraRules     = undefined
       , strategy       = undefined
       -- , navigation     = undefined
       , testGenerator  = Nothing
       -- , randomExercise = undefined
       , examples = [(scriptDifficulty, emptyState)]
       }