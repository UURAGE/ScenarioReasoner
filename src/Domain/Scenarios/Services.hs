{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Domain.Scenarios.Services where

import Data.Char
import Data.Maybe
import Data.List

import Ideas.Common.Library
import Ideas.Service.Types
import Ideas.Service.State

import Domain.Scenarios.Types hiding (State)
import Domain.Scenarios.Parser

customServices :: [Script] -> [Service]
customServices scripts = map (flip ($) scripts)
    [scenariolistS, scenarioinfoS, statementsinfoS, scoreS]

scenariolistS :: [Script] -> Service
scenariolistS scripts = makeService "scenarios.scenariolist"
    "Lists all available scenarios." $
    (scenariolist scripts) ::: (tList tScenarioInfo)

scenariolist :: [Script] -> [ScenarioInfo]
scenariolist = map getScenarioInfoFor

scenarioinfoS :: [Script] -> Service
scenarioinfoS scripts = makeService "scenarios.scenarioinfo"
    "Returns information about the scenario." $
    (scenarioinfo scripts) ::: tExercise .-> tScenarioInfo

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
                              (Tag "feedback"        tString)))))))))))
                              
        where pairify (ScenarioInfo a b c d e f g h i j k) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j ,k))))))))))
              
instance Show ScenarioInfo where
  show (ScenarioInfo id name desc diff b c m ps ss sf fb) = 
    show id   ++ "\n" ++ 
    show name ++ "\n" ++ 
    show desc ++ "\n" ++ 
    show diff ++ "\n" ++ 
    show b    ++ "\n" ++ 
    show c    ++ "\n" ++ 
    show m    ++ "\n" ++ 
    show ps   ++ "\n" ++ 
    show ss   ++ "\n" ++ 
    show sf   ++ "\n" ++ 
    show fb

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
          describeParameter param = ParameterInfo
                (parameterId param)
                (parameterName param)
                (parameterEmotion param)

statementsinfoS :: [Script] -> Service
statementsinfoS scripts = makeService "scenarios.statementsinfo"
    "Returns information for all statements of the scenario." $
    (statementsinfo scripts) ::: tExercise .-> (tList tStatementInfo)

type StatementText = (Either String [(String, String)])
tStatementText :: Type a StatementText
tStatementText = tString :|: (tList (tPair tString tString))

data StatementInfo = StatementInfo String
                                   String
                                   StatementText
                                   [String]
                                   (Maybe String)
                                   MediaInfo
                                   String --ending
                                   String --jump

tStatementInfo :: Type a StatementInfo
tStatementInfo = 
    Iso ((<-!) pairify) (Pair (Tag "id"  tString)
                        (Pair (Tag "type" tString)
                        (Pair (Tag "text" tStatementText)
                        (Pair (Tag "intents" (tList tString))
                        (Pair (Tag "feedback" (tMaybe tString))
                        (Pair (Tag "media" tMediaInfo)
                        (Pair (Tag "end" tString)
                              (Tag "jump" tString))))))))

        where pairify (StatementInfo a b c d e f g h) = (a, (b, (c, (d, (e, (f, (g, h)))))))


data MediaInfo = MediaInfo [(String, String)] [String]

tMediaInfo :: Type a MediaInfo
tMediaInfo = 
    Iso ((<-!) pairify) (Pair (Tag "visuals" (tList (tPair tString tString)))
                              (Tag "audios"  (tList tString)))
        where pairify (MediaInfo a b) = (a, b)


statementsinfo :: [Script] -> Exercise a -> [StatementInfo]
statementsinfo scripts ex = map statementInfo $ emptyOnFail $ getScriptStatements script
    where script = findScript "get info for" scripts ex
          statementInfo statement = StatementInfo
                (show $ createFullId script statement)
                (toIdTypeSegment $ fromJust $ getType statement)
                (either Left (Right . map showConversationTextTypeStringTuple) $
                    errorOnFail $ getText statement)
                (emptyOnFail $ getIntents statement)
                (errorOnFail $ getFeedback statement)
                (MediaInfo
                    (emptyOnFail $ getMediaVisuals statement)
                    (emptyOnFail $ getMediaAudios statement)
                )
                (head $ getEnd statement)--because monad
                (head $ getJump statement)
          emptyOnFail = fromMaybe []
          showConversationTextTypeStringTuple (ctt, s) = (map toLower $ show ctt, s)

{-          
readyS :: [Script] -> Service
readyS scripts = makeService "scenarios.finished"
    "Checks if the conversation is done" $
    (finished scripts) 
-}

data ScoreResult = ScoreResult Score 
                               [SubScore] 
                               (Maybe (Score, Score))
                               
type Score = Int
type SubScore = (String, String, Score)

tScoreResult :: Type a ScoreResult
tScoreResult =
    Iso ((<-!) pairify) (Pair (Tag "mainScore"          tInt)
                        (Pair (Tag "subScores"         (tList (tTuple3 tString tString tInt)))
                              (Tag "mainScoreExtremes" (tMaybe (tPair tInt tInt)))))
        where pairify (ScoreResult score subscores extremes) = (score, (subscores, extremes))
         
scoreS :: [Script] -> Service
scoreS scripts = makeService "scenarios.score"
    "Calculates the score of a given state." $
    (score scripts) ::: tState .-> tScoreResult

-- Type-customized result structure
score :: [Script] -> State a -> ScoreResult
score scripts fstate = ScoreResult mainScore subScores mainScoreExtremes
    where script = findScript "score" scripts $ exercise fstate
          state = (fromMaybe (error "Cannot score exercise: casting failed.") $
                      castFrom (exercise fstate) (stateTerm fstate))
          mainScore = calculateScore
              (fromMaybe (error "Cannot score exercise: no scoring function found.") $
                  getScriptScoringFunction script)
              (state)
          subScores = calculateSubScores (fromMaybe [] $ getScriptParameters script) state
          mainScoreExtremes = (errorOnFail $ getScriptScoreExtremes script)

findScript :: String -> [Script] -> Exercise a -> Script
findScript usage scripts ex =
    case filter (\testScript -> (getId testScript) == (getId ex)) scripts of
            [foundScript] -> foundScript
            _             ->
                error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a Scenario."

errorOnFail :: Maybe a -> a
errorOnFail = fromMaybe (error "failed...")