module Domain.Scenarios.Exercises where

import Control.Monad
import Data.List
import Data.Map(findWithDefault)
import System.Directory

import Ideas.Common.Library

import Domain.Scenarios.Types
import Domain.Scenarios.Parser
import Domain.Scenarios.Strategy

getExercises :: String ->  IO ([Exercise State], [Script])
getExercises scenarioId = do
    let scriptPath = "../../scenarios/scripts/" ++ (tail $ dropWhile (\x -> x/= '.') scenarioId) ++ ".xml" -- : The script directory.
    exercisePair <- getExercise scriptPath
    return $ (dummyExercise : [fst exercisePair], [snd exercisePair])

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
    let processParameter p = (parameterId p, parameterInitialValueOrZero p)
        initialState = fromList $ map processParameter scriptParameters --initial state for strategy generation
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
