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
main = do
    myExercises <- E.getExercises  
    defaultMain (ideasScenarios (map Some myExercises))

-- maindoc = makeDocumentation ideasScenarios "doc"

ideasScenarios :: [Some Exercise] -> DomainReasoner
ideasScenarios myExercises = (newDomainReasoner "ideas.scenarios")
      { exercises = myExercises
      , services  = (myServices (ideasScenariosStub myExercises))
      , views     = myViewList
      , aliases   = myAliases
      , scripts   = myScripts
      , testSuite = myTestSuite
      }

-- Emergency workaround
-- (cannot inject data into recursive definition)
ideasScenariosStub :: [Some Exercise] -> DomainReasoner
ideasScenariosStub myExercises = (newDomainReasoner "ideas.scenarios")
      { exercises = myExercises
      , views     = myViewList
      , aliases   = myAliases
      , scripts   = myScripts
      , testSuite = myTestSuite
      }

myServices :: DomainReasoner -> [Service]
myServices dr = S.customServices ++ metaServiceList dr ++ serviceList

myViewList :: [ViewPackage]
myViewList = []

myAliases :: [(Id, Id)]
myAliases = []

myScripts :: [(Id, FilePath)]
myScripts = []

myTestSuite :: TestSuite
myTestSuite = undefined
