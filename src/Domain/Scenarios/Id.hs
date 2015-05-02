{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
module Domain.Scenarios.Id where

import Data.Char(isLower, toLower)
import Data.List(intercalate)

import Ideas.Common.Library

import Domain.Scenarios.Globals(Script, applyToFirst)
import Domain.Scenarios.Parser

-- Definitions of Id instances and functions (Id is a data structure from the Ideas framework)

instance HasId Script where
    getId script = either error id $ do
                let scenario = scenarioMetaData (parseScenario script)
                let descr = scenarioDescription scenario
                let id = scenarioID scenario
                return $ describe descr $ "scenarios" # id
    changeId _ _ = error "The ID of a Script is determined externally."

instance HasId Scenario where
    getId (Scenario metadata _) = either error id $ do
                let id = scenarioID metadata
                let descr = scenarioDescription metadata
                return $ describe descr $ "scenarios" # id
    changeId _ _ = error "The ID of a Script is determined externally."

instance HasId Statement where
    getId statement = either error id $ do
                let statementId = statID statement
                let statementText = statText statement
                let statementDescription = either id (intercalate " // " . map snd) statementText
                return $ describe statementDescription $ newId statementId
    changeId _ _ = error "The ID of a Statement is determined externally."
    

-- | Creates the full ID for the given statement in the context of the given script.
createFullId :: Scenario -> Statement -> Id
createFullId scenario statement = scenarioID # typeSegment # statId
  where 
    scenarioID = getId scenario
    typeSegment = toIdTypeSegment $ statType statement
    statId = statID statement

-- | Returns the value to be used to represent a statement type in a rule ID.
toIdTypeSegment :: StatementType -> String
toIdTypeSegment = takeWhile isLower . applyToFirst toLower . show                    
                      
findScript :: String -> [Script] -> Exercise a -> Script
findScript usage scripts ex =
    case filter (\testScript -> (getId testScript) == (getId ex)) scripts of
            [foundScript] -> foundScript
            _             ->
                error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a Scenario."