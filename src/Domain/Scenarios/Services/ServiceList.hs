------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Domain.Scenarios.Services.ServiceList (customServices) where

import Ideas.Service.Types
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

-- Service that gives back the computed feedbackform defined in the xml
feedbackformS :: [FilePath] -> Service
feedbackformS fs = makeService "scenarios.feedbackform"
    "Gives detailed feedback for every parameter." $
    feedbackform fs ::: tState .-> tList (tPair tString tString)

-- Gives a list of scenario's together with all their info
scenariolistS :: [FilePath] -> Service
scenariolistS fs = makeService "scenarios.scenariolist"
    "Lists all available scenarios." $
    scenariolist fs ::: tList tScenarioInfo

-- Gives the info about a scenario
scenarioinfoS :: [FilePath] -> Service
scenarioinfoS fs = makeService "scenarios.scenarioinfo"
    "Returns information about the scenario." $
    scenarioinfo fs ::: tExercise .-> tScenarioInfo

-- Service that returns the calculated score 
-- using the scoring function defined in the xml
scoreS :: [FilePath] -> Service
scoreS fs = makeService "scenarios.score"
    "Calculates the score of a given state." $
    score fs ::: tState .-> tScoreResult
