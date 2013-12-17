module Domain.Scenarios.Services where

import Data.Maybe
import Data.List

import Ideas.Common.Library
import Ideas.Service.Types
import Ideas.Service.State

import Domain.Scenarios.Types (calculateScore, calculateSubScores, toIdTypeSegment)
import Domain.Scenarios.Parser

customServices :: [Script] -> [Service]
customServices scripts = [alldescriptionsS scripts, statementsinfoS scripts, scoreS scripts]

alldescriptionsS :: [Script] -> Service
alldescriptionsS scripts = deprecate $ makeService "scenarios.alldescriptions"
    "Returns the descriptions of all rules in the exercise." $
    (alldescriptions scripts) ::: typed

alldescriptions :: [Script] -> Exercise a -> [(String, String)]
alldescriptions scripts ex = map idAndDescription $ fromMaybe [] $ getScriptStatements script
    where script = findScript "describe" scripts ex
          scriptId = getId script
          idAndDescription statement = (show $ createId statement, createDescription statement)
          createId s = scriptId # [typeSegment s, idSegment s]
          typeSegment statement = toIdTypeSegment $ fromJust $ getType statement
          idSegment statement = show $ getId statement
          createDescription = fromMaybe "" . getText

statementsinfoS :: [Script] -> Service
statementsinfoS scripts = makeService "scenarios.statementsinfo"
    "Returns information for all statements of the scenario." $
    (statementsinfo scripts) ::: typed

statementsinfo :: [Script] -> Exercise a ->
    [(String, Either String [(String, String)], [String], [[String]])]
statementsinfo scripts ex = map statementInfo $ emptyOnFail $ getScriptStatements script
    where script = findScript "describe" scripts ex
          scriptId = getId script
          statementInfo statement =
                ( show $ createId statement
                , Left $ createDescription statement
                , emptyOnFail $ getIntents statement
                , [ emptyOnFail $ getMedia "video" statement
                  , emptyOnFail $ getMedia "image" statement
                  , emptyOnFail $ getMedia "audio" statement
                  ]
                )
          createId s = scriptId # [typeSegment s, idSegment s]
          typeSegment statement = toIdTypeSegment $ fromJust $ getType statement
          idSegment statement = show $ getId statement
          createDescription = fromMaybe "" . getText
          emptyOnFail = fromMaybe []

scoreS :: [Script] -> Service
scoreS scripts = makeService "scenarios.score"
    "Calculates the score of a given state." $
    (score scripts) ::: typed

score :: [Script] -> State a -> (Int, [(String, String, Int)])
score scripts fstate = (mainScore, subScores)
    where script = findScript "score" scripts $ exercise fstate
          state = (fromMaybe (error "Cannot score exercise: casting failed.") $
                      castFrom (exercise fstate) (stateTerm fstate))
          mainScore = calculateScore
              (fromMaybe (error "Cannot score exercise: no scoring function found.") $
                  getScriptScoringFunction script)
              (state)
          subScores = calculateSubScores (fromMaybe [] $ getScriptParameters script) state

findScript :: String -> [Script] -> Exercise a -> Script
findScript usage scripts ex =
    case filter (\testScript -> (getId testScript) == (getId ex)) scripts of
            [foundScript] -> foundScript
            _             ->
                error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a Scenario."