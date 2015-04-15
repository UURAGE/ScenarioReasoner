module Domain.Scenarios.Exercises where

import Control.Monad
import Data.List
import Data.Map(findWithDefault, fromList)
import System.Directory

import Ideas.Common.Library

import Domain.Scenarios.Globals(ScriptElement, parameterId, parameterInitialValueOrZero)
import Domain.Scenarios.Strategy(makeStrategy)
import Domain.Scenarios.Parser
import Domain.Scenarios.ScriptState

getExercises :: String ->  IO ([Exercise ScriptState], [ScriptElement])
getExercises scenarioId = do
    let scriptPath = "../../scenarios/scripts/" ++ (tail $ dropWhile (\x -> x/= '.') scenarioId) ++ ".xml" -- : The script directory.
    exercisePair <- getExercise scriptPath
    return $ (dummyExercise : [fst exercisePair], [snd exercisePair])

getExercise :: String -> IO (Exercise ScriptState, ScriptElement)
getExercise path = do
    scriptElem <- parseScriptElement path
    exercise <- exerciseFromScript scriptElem
    return (exercise, scriptElem)

exerciseFromScript :: Monad m => ScriptElement -> m (Exercise ScriptState)
exerciseFromScript scriptElem = do
    let scriptMetaData = (\Script metaData _ -> metaData) parseScript scriptElem
    scriptStrategy <- makeStrategy scriptElem
    let difficulty = scriptDifficulty scriptMetaData
    let parameters = scriptParameters scriptMetaData
    let processParameter p = (parameterId p, parameterInitialValueOrZero p)
        initialState = (fromList (map processParameter parameters), "") :: ScriptState --initial state for strategy generation
    return makeExercise
       { exerciseId     = getId scriptElem
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
dummyExercise :: Exercise ScriptState
dummyExercise = makeExercise { exerciseId = newId "scenarios-dummy" }
