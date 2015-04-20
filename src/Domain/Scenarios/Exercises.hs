module Domain.Scenarios.Exercises where

import Control.Monad
import Data.List
import Data.Map(findWithDefault, fromList)
import System.Directory

import Ideas.Common.Library

import Domain.Scenarios.Globals(Script, parameterId, parameterInitialValueOrZero)
import Domain.Scenarios.Strategy(makeStrategy)
import Domain.Scenarios.Parser
import Domain.Scenarios.ScenarioState

getExercises :: String ->  IO ([Exercise ScenarioState], [Script])
getExercises scenarioId = do
    let scriptPath = "../../scenarios/scripts/" ++ (tail $ dropWhile (\x -> x/= '.') scenarioId) ++ ".xml" -- : The script directory.
    exercisePair <- getExercise scriptPath
    return $ (dummyExercise : [fst exercisePair], [snd exercisePair])

getExercise :: String -> IO (Exercise ScenarioState, Script)
getExercise path = do
    script <- parseScript path
    exercise <- exerciseFromScript script
    return (exercise, script)

exerciseFromScript :: Monad m => Script -> m (Exercise ScenarioState)
exerciseFromScript script = do
    let scriptMetaData = parseMetaData script
    scriptStrategy <- makeStrategy script
    let difficulty = scenarioDifficulty scriptMetaData
    let parameters = scenarioParameters scriptMetaData
    let processParameter p = (parameterId p, parameterInitialValueOrZero p)
        initialState = (fromList (map processParameter parameters), "") :: ScenarioState --initial state for strategy generation
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
       , examples = [(difficulty, initialState)]
       }

       
-- A dummy exercise necessary for use with general services
dummyExercise :: Exercise ScenarioState
dummyExercise = makeExercise { exerciseId = newId "scenarios-dummy" }
