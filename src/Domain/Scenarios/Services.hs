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
    (scenariolist scripts) ::: typed

scenariolist :: [Script] -> [ScenarioInfo]
scenariolist = map getScenarioInfoFor

scenarioinfoS :: [Script] -> Service
scenarioinfoS scripts = makeService "scenarios.scenarioinfo"
    "Returns information about the scenario." $
    (scenarioinfo scripts) ::: typed

data ScenarioInfo = ScenarioInfo String
                                 String
                                 String
                                 Difficulty
                                 (Maybe String)
                                 (Maybe String)
                                 (Maybe String)
                                 [ParameterInfo]
instance Typed a ScenarioInfo where
    typed = Iso ((<-!) pairify) (Pair (Tag "id" typed)
                                (Pair (Tag "name" typed)
                                (Pair (Tag "description" typed)
                                (Pair (tag "difficulty" typed)
                                (Pair (Tag "bannerImage" typed)
                                (Pair (Tag "characterImage" typed)
                                (Pair (Tag "model" typed)
                                      (Tag "parameters" typed))))))))
        where pairify (ScenarioInfo a b c d e f g h) = (a, (b, (c, (d, (e, (f, (g, h)))))))
              tag s (Tag _ t) = Tag s t
              tag s t         = Tag s t

data ParameterInfo = ParameterInfo String
                                   String
                                   (Maybe String)
instance Typed a ParameterInfo where
    typed = Iso ((<-!) pairify) (Pair (Tag "id" typed)
                                (Pair (Tag "name" typed)
                                      (Tag "emotion" typed)))
        where pairify (ParameterInfo a b c) = (a, (b, c))

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
    where scriptId = show $ getId script
          scriptName = errorOnFail $ getScriptName script
          scriptDescription = errorOnFail $ getScriptDescription script
          scriptDifficulty = errorOnFail $ getScriptDifficulty script
          scriptBannerImage = errorOnFail $ getScriptBannerImage script
          scriptCharacterImage = errorOnFail $ getScriptCharacterImage script
          scriptModel = errorOnFail $ getScriptModel script
          scriptParameters = map describeParameter $ errorOnFail $ getScriptParameters script
          describeParameter param = ParameterInfo
                (parameterId param)
                (parameterName param)
                (parameterEmotion param)

statementsinfoS :: [Script] -> Service
statementsinfoS scripts = makeService "scenarios.statementsinfo"
    "Returns information for all statements of the scenario." $
    (statementsinfo scripts) ::: typed

type StatementText = (Either String [(String, String)])
data StatementInfo = StatementInfo String
                                   String
                                   StatementText
                                   [String]
                                   (Maybe String)
                                   MediaInfo
instance Typed a StatementInfo where
    typed = Iso ((<-!) pairify) (Pair (Tag "id" typed)
                                (Pair (Tag "type" typed)
                                (Pair (Tag "text" typed)
                                (Pair (Tag "intents" typed)
                                (Pair (Tag "feedback" typed)
                                      (Tag "media" typed))))))
        where pairify (StatementInfo a b c d e f) = (a, (b, (c, (d, (e, f)))))

data MediaInfo = MediaInfo [(String, String)] [String]
instance Typed a MediaInfo where
    typed = Iso ((<-!) pairify) (Pair (Tag "visuals" typed)
                                      (Tag "audios" typed))
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
          emptyOnFail = fromMaybe []
          showConversationTextTypeStringTuple (ctt, s) = (map toLower $ show ctt, s)

scoreS :: [Script] -> Service
scoreS scripts = makeService "scenarios.score"
    "Calculates the score of a given state." $
    (score scripts) ::: typed

-- Legacy (non-Typed-customised) result structure because of existing usage.
-- Maybe [Int] instead of Maybe (Int, Int) because the latter is merged into
-- the main structure by EncoderJSON.
score :: [Script] -> State a -> (Int, [(String, String, Int)], Maybe [Int])
score scripts fstate = (mainScore, subScores, mainScoreExtremes)
    where script = findScript "score" scripts $ exercise fstate
          state = (fromMaybe (error "Cannot score exercise: casting failed.") $
                      castFrom (exercise fstate) (stateTerm fstate))
          mainScore = calculateScore
              (fromMaybe (error "Cannot score exercise: no scoring function found.") $
                  getScriptScoringFunction script)
              (state)
          subScores = calculateSubScores (fromMaybe [] $ getScriptParameters script) state
          mainScoreExtremes = (errorOnFail $ getScriptScoreExtremes script) >>= return . tupleToList
          tupleToList (a, b) = [a, b]

findScript :: String -> [Script] -> Exercise a -> Script
findScript usage scripts ex =
    case filter (\testScript -> (getId testScript) == (getId ex)) scripts of
            [foundScript] -> foundScript
            _             ->
                error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a Scenario."

errorOnFail :: Maybe a -> a
errorOnFail = fromMaybe (error "failed...")