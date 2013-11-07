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
import qualified Domain.Scenarios.Services as S
import qualified Domain.Scenarios.Exercises as E

main :: IO ()
main = createDomainReasoner >>= defaultMain

maindoc :: IO ()
maindoc = createDomainReasoner >>= flip makeDocumentation "doc"

createDomainReasoner :: IO DomainReasoner
createDomainReasoner = do
    flatExercises <- E.getExercises
    let myExercises = map Some flatExercises
    let ideasScenarios  = (newDomainReasoner "ideas.scenarios")
            { exercises = myExercises
            , services  = myServices
            , views     = myViewList
            , aliases   = myAliases
            , scripts   = myScripts
            , testSuite = myTestSuite
            }
        myServices = S.customServices ++ metaServiceList ideasScenarios ++ serviceList
    return ideasScenarios

myViewList :: [ViewPackage]
myViewList = []

myAliases :: [(Id, Id)]
myAliases = []

myScripts :: [(Id, FilePath)]
myScripts = []

myTestSuite :: TestSuite
myTestSuite = undefined
