{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Domain.Scenarios.Services.ScenarioInfo where

import Data.Map as M

import Ideas.Common.Library

import Domain.Scenarios.Parser
import Domain.Scenarios.TypeDefs(ID, Name, Emotion)

data ScenarioInfo = ScenarioInfo 
        { scenarioID             :: ID
        , scenarioName           :: Name
        , scenarioDescription    :: String
        , scenarioDifficulty     :: Difficulty 
        , scenarioBannerImage    :: Maybe ID
        , scenarioCharacterImage :: Maybe ID
        , scenarioModel          :: Maybe ID 
        , scenarioParameters     :: [ParameterInfo]
        , scenarioLocation       :: Name
        , scenarioSettings       :: [Toggle]
        }
            
instance Show ScenarioInfo where
  show (ScenarioInfo id name desc diff fb bi ci model ps lc ss) = 
    show id   ++ "\n" ++ show name  ++ "\n" ++ show desc ++ "\n" ++ 
    show diff ++ "\n" ++ show fb    ++ "\n" ++ show bi   ++ "\n" ++ 
    show ci   ++ "\n" ++ show model ++ "\n" ++ show ps   ++ "\n" ++ 
    show lc   ++ "\n" ++ show ss    ++ "\n" 

data ParameterInfo = ParameterInfo ID
                                   Name
                                   (Maybe Emotion)
    
instance Show ParameterInfo where
  show (ParameterInfo id name emotion) = show id ++ ", " ++ show name ++ ", " ++ show emotion
  
data Toggle = Toggle Name Bool
    deriving (Show)

toggleList :: [Name]
toggleList = ["showscore", "showfeedback", "feedback"]
  
  -- scenariolist service
scenariolist :: [Script] -> [ScenarioInfo]
scenariolist = map getScenarioInfoFor

-- scenarioinfo service
scenarioinfo :: [Script] -> Exercise a -> ScenarioInfo
scenarioinfo scripts ex = getScenarioInfoFor $ findScript "get info for" scripts ex

getScenarioInfoFor :: Script -> ScenarioInfo
getScenarioInfoFor script = ScenarioInfo
                (scriptId)
                (scriptName)
                (scriptDescription)
                (scriptDifficulty)
                (scriptShowScore)
                (scriptShowFeedback)
                (scriptFeedback)--true: feedback in game and at the end, false: only at the end
                (scriptBannerImage)
                (scriptCharacterImage)
                (scriptModel)
                (scriptParameters)
                (scriptLocation)
    where scriptId = show $ getId script
          scriptName = errorOnFail $ getScriptName script
          scriptDescription = errorOnFail $ getScriptDescription script
          scriptDifficulty = errorOnFail $ getScriptDifficulty script
          scriptBannerImage = errorOnFail $ getScriptBannerImage script
          scriptCharacterImage = errorOnFail $ getScriptCharacterImage script
          scriptModel = errorOnFail $ getScriptModel script
          scriptParameters = map describeParameter $ errorOnFail $ getScriptParameters script
          scriptShowScore = errorOnFail $ getScriptShowScore script
          scriptShowFeedback = errorOnFail $ getScriptShowFeedback script
          scriptFeedback = errorOnFail $ getScriptFeedback script
          scriptLocation = errorOnFail $ getScriptLocation script
          describeParameter param = ParameterInfo
                (parameterId param)
                (parameterName param)
                (parameterEmotion param)