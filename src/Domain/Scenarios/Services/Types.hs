------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Domain.Scenarios.Services.Types where

import Ideas.Common.Library
import Ideas.Service.Types

import Domain.Scenarios.Globals
import Domain.Scenarios.ScoringFunction(SubScore)

-- | Ideas Framework type definitions for sending an object through a service

-- ScenarioInfo types -----------------------------------------------------------------------------------------------------------

data ScenarioInfo = ScenarioInfo ID
                                 Name
                                 String           -- Description
                                 Difficulty 
                                 (Maybe ID)       -- BannerImage
                                 (Maybe ID)       -- CharacterImage
                                 (Maybe ID)       -- Model
                                 [ParameterInfo]
                                 Name             -- Location
                                 Name             -- Pet
                                 [Toggle]
                                 
tScenarioInfo :: Type a ScenarioInfo
tScenarioInfo = 
    Iso ((<-!) pairify) (Pair (Tag "id"              tString)
                        (Pair (Tag "name"            tString)
                        (Pair (Tag "description"     tString)
                        (Pair                        tDifficulty    
                        (Pair (Tag "bannerImage"    (tMaybe tString))
                        (Pair (Tag "characterImage" (tMaybe tString))
                        (Pair (Tag "model"          (tMaybe tString))
                        (Pair (Tag "parameters"     (tList tParameterInfo))
                        (Pair (Tag "location"        tString)
                        (Pair (Tag "pet"             tString)
                              (Tag "toggles"        (tList tToggle))))))))))))                             
      where 
        pairify (ScenarioInfo id name descr diff bi ci model ps loc pet ts) = 
            (id, (name, (descr, (diff, (bi, (ci, (model, (ps, (loc, (pet, ts))))))))))
                       

instance Show ScenarioInfo where
  show (ScenarioInfo id name desc diff bi ci model ps lc pet ss) = 
    show id    ++ "\n" ++ show name  ++ "\n" ++ show desc ++ "\n" ++ 
    show diff  ++ "\n" ++ show bi    ++ "\n" ++ show ci   ++ "\n" ++ 
    show model ++ "\n" ++ show ps    ++ "\n" ++ show lc   ++ "\n" ++ 
    show pet   ++ "\n" ++ show ss    ++ "\n"   

data ParameterInfo = ParameterInfo ID
                                   Name
                                   String -- Description


instance Show ParameterInfo where
  show (ParameterInfo id name descr) = show id ++ ", " ++ show name ++ ", " ++ show descr
 
tParameterInfo = Iso ((<-!) pairify) (Pair (Tag "id"            tString)
                                     (Pair (Tag "name"          tString)
                                           (Tag "description"   tString)))                                    
        where pairify (ParameterInfo id name descr) = (id, (name, descr))
        
tToggle :: Type a Toggle
tToggle = Iso ((<-!) pairify) (Pair (Tag "name" tString) 
                                    (Tag "bool" tBool)) 
  where pairify (Toggle name boolean) = (name, boolean)
    
-- ScoreResult type -------------------------------------------------------------------------------------------------------------

data ScoreResult = ScoreResult Score           -- Total score as a percentage
                               [SubScore]      -- All subscores for all scored parameters
                               (Maybe [Score]) -- Extremes of the total score

tScoreResult :: Type a ScoreResult
tScoreResult =
    Iso ((<-!) pairify) (Pair (Tag "mainscore"          tInt)
                        (Pair (Tag "subscores"         (tList (tTuple3 tString tString tInt)))
                              (Tag "extremes"          (tMaybe (tList tInt)))))
        where pairify (ScoreResult score subscores extremes) = (score, (subscores, extremes))        
