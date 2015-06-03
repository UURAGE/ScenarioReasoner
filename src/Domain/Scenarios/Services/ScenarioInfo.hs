module Domain.Scenarios.Services.ScenarioInfo where

import System.FilePath(FilePath)

import Ideas.Common.Library

import Domain.Scenarios.ScoringFunction
import Domain.Scenarios.Parser(parseScenario, parseScript, findScript)
import Domain.Scenarios.Globals
import Domain.Scenarios.Scenario

data ScenarioInfo = ScenarioInfo ID
                                 Name
                                 String           -- Description
                                 Difficulty 
                                 (Maybe ID)       -- BannerImage
                                 (Maybe ID)       -- CharacterImage
                                 (Maybe ID)       -- Model
                                 [ParameterInfo]
                                 Name             -- Location
                                 [Toggle]
            
instance Show ScenarioInfo where
  show (ScenarioInfo id name desc diff bi ci model ps lc ss) = 
    show id    ++ "\n" ++ show name  ++ "\n" ++ show desc ++ "\n" ++ 
    show diff  ++ "\n" ++ show bi    ++ "\n" ++ show ci   ++ "\n" ++ 
    show model ++ "\n" ++ show ps    ++ "\n" ++ show lc   ++ "\n" ++ 
    show ss    ++ "\n" 

data ParameterInfo = ParameterInfo ID
                                   Name
    
instance Show ParameterInfo where
  show (ParameterInfo id name) = show id ++ ", " ++ show name
  
-- Scenariolist service: lists all info for each scenario
scenariolist :: [FilePath] -> [ScenarioInfo]
scenariolist = map (getScenarioInfo . parseScenario . parseScript)

-- Scenarioinfo service: shows the info for a specific scenario (exercise)
scenarioinfo :: [FilePath] -> Exercise a -> ScenarioInfo
scenarioinfo fs ex = getScenarioInfo (parseScenario (findScript "get info for" fs ex))

getScenarioInfo :: Scenario -> ScenarioInfo
getScenarioInfo scenario@(Scenario metadata _) = ScenarioInfo
                (show (getId scenario))
                (scenarioName           metadata)
                (scenarioDescription    metadata)
                (scenarioDifficulty     metadata)
                (scenarioBannerImage    metadata)
                (scenarioCharacterImage metadata)
                (scenarioModel          metadata)
                (map describeParameter (scenarioParameters metadata))
                (scenarioLocation       metadata)
                (scenarioToggles        metadata)
  where 
    describeParameter param = ParameterInfo
        (parameterId      param)
        (parameterName    param)