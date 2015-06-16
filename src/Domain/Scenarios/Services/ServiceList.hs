module Domain.Scenarios.Services.ServiceList where

import System.FilePath(FilePath)

import Ideas.Common.Library
import Ideas.Service.Types
import Ideas.Service.State
import Ideas.Service.BasicServices (tStepInfo)

import Domain.Scenarios.Services.AdaptedServices(allfirsts)
import Domain.Scenarios.Services.ExtraServices(feedbackform, scenariolist, scenarioinfo, score)
import Domain.Scenarios.Services.Types

-- A list of all custom services available
customServices :: [FilePath] -> [Service]
customServices fs = [allfirstsS] ++ map ($ fs)
    [feedbackformS, scenariolistS, scenarioinfoS, scoreS]
   
-- Adapted allfirsts   
allfirstsS :: Service
allfirstsS = makeService "scenarios.allfirsts"
   "Returns all next steps that are suggested by the strategy. See the \
   \onefirst service to get only one suggestion. For each suggestion, a new \
   \state, the rule used, and the location where the rule was applied are \
   \returned." $
   allfirsts ::: tState .-> tError (tList (tPair tStepInfo tState))
    
feedbackformS :: [FilePath] -> Service
feedbackformS fs = makeService "scenarios.feedbackform"
    "Gives detailed feedback for every parameter." $
    feedbackform fs ::: tState .-> tList (tPair tString tString)

scenariolistS :: [FilePath] -> Service
scenariolistS fs = makeService "scenarios.scenariolist"
    "Lists all available scenarios." $
    scenariolist fs ::: tList tScenarioInfo

scenarioinfoS :: [FilePath] -> Service
scenarioinfoS fs = makeService "scenarios.scenarioinfo"
    "Returns information about the scenario." $
    scenarioinfo fs ::: tExercise .-> tScenarioInfo

scoreS :: [FilePath] -> Service
scoreS fs = makeService "scenarios.score"
    "Calculates the score of a given state." $
    score fs ::: tState .-> tScoreResult
    


