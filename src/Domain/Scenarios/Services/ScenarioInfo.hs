{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Domain.Scenarios.Services.ScenarioInfo where

import Ideas.Common.Library

import Domain.Scenarios.ScoringFunction
import Domain.Scenarios.Parser
import Domain.Scenarios.TypeDefs(ID, Name, Emotion)

data ScenarioInfo = ScenarioInfo ID
                                 Name
                                 String          --Description
                                 Difficulty 
                                 (Maybe ID)       --BannerImage
                                 (Maybe ID)       --CharacterImage
                                 (Maybe ID)       --Model
                                 [ParameterInfo]
                                 Name            --Location
                                 [Toggle]
            
instance Show ScenarioInfo where
  show (ScenarioInfo id name desc diff bi ci model ps lc ss) = 
    show id    ++ "\n" ++ show name  ++ "\n" ++ show desc ++ "\n" ++ 
    show diff  ++ "\n" ++ show bi    ++ "\n" ++ show ci   ++ "\n" ++ 
    show model ++ "\n" ++ show ps    ++ "\n" ++ show lc   ++ "\n" ++ 
    show ss    ++ "\n" 

data ParameterInfo = ParameterInfo ID
                                   Name
                                   (Maybe Emotion)
    
instance Show ParameterInfo where
  show (ParameterInfo id name emotion) = show id ++ ", " ++ show name ++ ", " ++ show emotion
  
-- scenariolist service
scenariolist :: [ScriptElem] -> [ScenarioInfo]
scenariolist = map (getScenarioInfo . parseScript)

-- scenarioinfo service
scenarioinfo :: [ScriptElem] -> Exercise a -> ScenarioInfo
scenarioinfo scripts ex = getScenarioInfo (parseScript (findScript "get info for" scripts ex))

getScenarioInfo :: Script -> ScenarioInfo
getScenarioInfo script@(Script metadata _) = ScenarioInfo
                (show $ getId script)
                (scriptName metadata)
                (scriptDescription metadata)
                (scriptDifficulty metadata)
                (scriptBannerImage metadata)
                (scriptCharacterImage metadata)
                (scriptModel metadata)
                (map describeParameter (scriptParameters metadata))
                (scriptLocation metadata)
                (scriptToggles metadata)
  where 
    describeParameter param = ParameterInfo
        (parameterId param)
        (parameterName param)
        (parameterEmotion param)