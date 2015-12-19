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
maindoc = do
    drTuple <- scenarioReasoner 
    let dr = fst drTuple
    makeDocumentation dr "doc"

scenarioReasoner :: IO (DomainReasoner, DomainReasoner)
scenarioReasoner = do
    exerciseList <- E.exercises    
    let sps = map snd exerciseList
    let exs = map (Some . fst) exerciseList
        dr  = (newDomainReasoner "ideas.scenarios")
            { exercises = exs
            , services  = S.customServices sps ++ metaServiceList dr ++ serviceList
            }
            
    tExerciseList <- E.testingExercises
    let tsps = map snd tExerciseList
    let texs = map (Some . fst) tExerciseList
        tdr = (newDomainReasoner "ideas.scenarios.test")
            { exercises = texs
            , services  = S.customServices tsps ++ metaServiceList dr ++ serviceList
            }
            
    return (dr, tdr)