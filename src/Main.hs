module Main where


import Control.Arrow
import Ideas.Common.Exercise
import Ideas.Common.Id
import Ideas.Common.Utils (Some(..))
import Ideas.Common.Utils.TestSuite
import Ideas.Common.Library
import Ideas.Main.Default
import Ideas.Main.Documentation
import Ideas.Service.DomainReasoner
import Ideas.Service.ServiceList
import Ideas.Service.Types (Service)
import qualified Domain.Scenarios.Services.ServiceList as S
import qualified Domain.Scenarios.Exercises as E

-- changes were made in Ideas.Main.Default to the createDomainReasoner functions to adjust to load the scenario id 
-- should not be adjusted, instead request exerciselist with list of scenarioIDs.. and send scenario id instead
main :: IO ()
main = scenarioReasoner >>= defaultMain

maindoc :: IO ()
maindoc = scenarioReasoner >>= flip makeDocumentation "doc"

scenarioReasoner :: IO DomainReasoner
scenarioReasoner = do
    exerciselist <- E.exercises
    let scenarioPaths = map snd exerciselist
    let exs = map (Some . fst) exerciselist
        dr  = (newDomainReasoner "ideas.scenarios")
            { exercises = exs
            , services  = S.customServices scenarioPaths ++ metaServiceList dr ++ serviceList
            }
    return dr