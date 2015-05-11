module Domain.Scenarios.Services.ScenarioInfo where

import Ideas.Common.Library

import Domain.Scenarios.ScoringFunction
import Domain.Scenarios.Parser
import Domain.Scenarios.Globals
import Domain.Scenarios.Id(findScript)

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
scenariolist :: [Script] -> [ScenarioInfo]
scenariolist = map (getScenarioInfo . parseScenario)

-- Scenarioinfo service: shows the info for a specific scenario (exercise)
scenarioinfo :: [Script] -> Exercise a -> ScenarioInfo
scenarioinfo scripts ex = getScenarioInfo (parseScenario (findScript "get info for" scripts ex))

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