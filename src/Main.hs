------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Main where

import Ideas.Main.Default
import Ideas.Main.Documentation
import qualified Domain.Scenarios.Services.ServiceList as S
import qualified Domain.Scenarios.Exercises as E

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