module Domain.Scenarios.Exercises where

import Control.Monad
import Data.List
import System.Directory

import Ideas.Common.Library

import Domain.Scenarios.Types
import Domain.Scenarios.Parser
import Domain.Scenarios.Strategy

getExercises :: IO ([Exercise State], [Script])
getExercises = do
    let directoryPath = "../../scenarios/scripts/"
    directoryContents <- getDirectoryContents directoryPath
    let scriptFiles = map (directoryPath++) $ filter (isSuffixOf ".xml") $ directoryContents
    exerciseScriptPairs <- mapM getExercise scriptFiles
    let (exercises, scripts) = unzip exerciseScriptPairs
    return $ (dummyExercise : exercises, scripts)

getExercise :: String -> IO (Exercise State, Script)
getExercise path = do
    script <- parseScript path
    exercise <- exerciseFromScript script
    return (exercise, script)

exerciseFromScript :: Monad m => Script -> m (Exercise State)
exerciseFromScript script = do
    scriptDifficulty <- getScriptDifficulty script
    scriptStrategy <- makeStrategy script
    scriptParameters <- getScriptParameters script
    let initialState = fromList [(i, v) |
            Parameter {parameterId = i, parameterInitialValue = Just v} <- scriptParameters]
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
       , examples = [(scriptDifficulty, initialState)]
       }

-- A dummy exercise necessary for use with general services
dummyExercise :: Exercise State
dummyExercise = makeExercise { exerciseId = newId "scenarios-dummy" }
