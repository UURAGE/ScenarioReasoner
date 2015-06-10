module Domain.Scenarios.Types where

import Ideas.Common.Library
import Ideas.Service.Types

import Domain.Scenarios.Globals

import Domain.Scenarios.Services.ScenarioInfo
import Domain.Scenarios.Services.Score

-- | Ideas Framework type definitions for sending an object through a service
-- | for more explanation see Ideas.Service.Types and the documentation 

-- ScenarioInfo types -----------------------------------------------------------------------------------------------------------
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
                              (Tag "toggles"        (tList tToggle)))))))))))                             
      where 
        pairify (ScenarioInfo id name descr diff bi ci model ps loc ts) = 
            (id, (name, (descr, (diff, (bi, (ci, (model, (ps, (loc, ts)))))))))
                       
tParameterInfo :: Type a ParameterInfo
tParameterInfo = Iso ((<-!) pairify) (Pair (Tag "id"            tString)
                                     (Pair (Tag "name"          tString)
                                           (Tag "description"   tString)))                                    
        where pairify (ParameterInfo id name descr) = (id, (name, descr))
        
tToggle :: Type a Toggle
tToggle = Iso ((<-!) pairify) (Pair (Tag "name" tString) 
                                    (Tag "bool" tBool)) 
  where pairify (Toggle name boolean) = (name, boolean)
    
-- ScoreResult type -------------------------------------------------------------------------------------------------------------
tScoreResult :: Type a ScoreResult
tScoreResult =
    Iso ((<-!) pairify) (Pair (Tag "mainscore"          tInt)
                        (Pair (Tag "subscores"         (tList (tTuple3 tString tString tInt)))
                              (Tag "extremes"          (tMaybe (tList tInt)))))
        where pairify (ScoreResult score subscores extremes) = (score, (subscores, extremes))        
