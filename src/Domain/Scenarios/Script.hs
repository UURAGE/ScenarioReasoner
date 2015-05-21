{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
module Domain.Scenarios.Script where

import Ideas.Common.Library
import Ideas.Text.XML

import Domain.Scenarios.Globals(Script, statType)
import Domain.Scenarios.Scenario
import Domain.Scenarios.Parser(parseScenario)

instance HasId Script where
    getId script = either error id $ do
                let scenario = scenarioMetaData (parseScenario script)
                let descr = scenarioDescription scenario
                let id = scenarioID scenario
                return $ describe descr $ "scenarios" # id
    changeId _ _ = error "The ID of a Script is determined externally."
    
findScript :: String -> [Script] -> Exercise a -> Script
findScript usage scripts ex =
    case filter (\testScript -> getId testScript == getId ex) scripts of
            [foundScript] -> foundScript
            _             ->
                error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a scenario."                