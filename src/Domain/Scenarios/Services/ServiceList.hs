{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Services.ServiceList (customServiceList, metaServiceList, serviceList) where

import Ideas.Common.Id
import Ideas.Service.DomainReasoner
import Ideas.Service.Types
import qualified Ideas.Service.ServiceList as S
import Ideas.Service.BasicServices (tStepInfo)

import Domain.Scenarios.Scenario
import Domain.Scenarios.Services.AdaptedServices(allfirsts)
import Domain.Scenarios.Services.ExtraServices(scenariolist, scenarioinfo)
import Domain.Scenarios.Services.Types

-- A list of all custom services available
customServiceList :: [(Id, Scenario)] -> [Service]
customServiceList fs = [allfirstsS] ++ map ($ fs)
    [scenariolistS, scenarioinfoS]

-- Meta-services for the given domain reasoner
metaServiceList :: DomainReasoner -> [Service]
metaServiceList = S.metaServiceList

-- Filtered Ideas serviceList, because we have our own adapted allfirsts
serviceList :: [Service]
serviceList = filter (\s -> getId s /= "basic" # "allfirsts") S.serviceList

-- Adapted allfirsts
allfirstsS :: Service
allfirstsS = makeService "scenarios.allfirsts"
   "Returns all next steps that are suggested by the strategy. See the \
   \onefirst service to get only one suggestion. For each suggestion, a new \
   \state, the rule used, and the location where the rule was applied are \
   \returned." $
   allfirsts ::: tState .-> tError (tList (tPair tStepInfo tState))

-- Gives a list of scenario's together with all their info
scenariolistS :: [(Id, Scenario)] -> Service
scenariolistS fs = makeService "scenarios.scenariolist"
    "Lists all available scenarios." $
    scenariolist fs ::: tList tScenarioInfo

-- Gives the info about a scenario
scenarioinfoS :: [(Id, Scenario)] -> Service
scenarioinfoS fs = makeService "scenarios.scenarioinfo"
    "Returns information about the scenario." $
    scenarioinfo fs ::: tExercise .-> tScenarioInfo
