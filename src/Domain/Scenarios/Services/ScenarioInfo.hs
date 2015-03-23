{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Domain.Scenarios.Services.ScenarioInfo where

import Ideas.Common.Library
import Ideas.Service.Types

import Domain.Scenarios.Types(errorOnFail, parameterId, parameterName, parameterEmotion)
import Domain.Scenarios.Parser

data ScenarioInfo = ScenarioInfo String
                                 String
                                 String
                                 Difficulty
                                 (Maybe String)
                                 (Maybe String)
                                 (Maybe String)
                                 [ParameterInfo]
                                 String
                                 String
                                 String
                                 String
                                 
tScenarioInfo :: Type a ScenarioInfo
tScenarioInfo = 
    Iso ((<-!) pairify) (Pair (Tag "id"              tString)
                        (Pair (Tag "name"            tString)
                        (Pair (Tag "description"     tString)
                        (Pair                        tDifficulty
                        (Pair (Tag "bannerImage"    (tMaybe tString))
                        (Pair (Tag "characterImage" (tMaybe tString))
                        (Pair (Tag "model"          (tMaybe tString))
                        (Pair (Tag "parameters"     (tList tParameterInfo))  
                        (Pair (Tag "showscore"       tString)
                        (Pair (Tag "showfeedback"    tString)                          
                        (Pair (Tag "feedback"        tString)
                              (Tag "location"        tString))))))))))))
                              
        where pairify (ScenarioInfo a b c d e f g h i j k l) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j ,(k, l)))))))))))
              
instance Show ScenarioInfo where
  show (ScenarioInfo id name desc diff bim cim mod ps ss sf fb loc) = 
    show id   ++ "\n" ++ 
    show name ++ "\n" ++ 
    show desc ++ "\n" ++ 
    show diff ++ "\n" ++ 
    show bim  ++ "\n" ++ 
    show cim  ++ "\n" ++ 
    show mod  ++ "\n" ++ 
    show ps   ++ "\n" ++ 
    show ss   ++ "\n" ++ 
    show sf   ++ "\n" ++ 
    show fb   ++ "\n" ++
    show loc

data ParameterInfo = ParameterInfo String
                                   String
                                   (Maybe String)
                                   
tParameterInfo :: Type a ParameterInfo
tParameterInfo = Iso ((<-!) pairify) (Pair (Tag "id"       tString)
                                     (Pair (Tag "name"     tString)
                                           (Tag "emotion" (tMaybe tString))))
                                           
        where pairify (ParameterInfo a b c) = (a, (b, c))

instance Show ParameterInfo where
  show (ParameterInfo a b c) = show a ++ ", " ++ show b ++ ", " ++ show c
  
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
                (scriptBannerImage)
                (scriptCharacterImage)
                (scriptModel)
                (scriptParameters)
                (scriptShowScore)
                (scriptShowFeedback)
                (scriptFeedback)--true: feedback in game and at the end, false: only at the end
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