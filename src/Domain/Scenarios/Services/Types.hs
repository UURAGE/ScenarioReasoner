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

data ScenarioInfo = ScenarioInfo Name
                                 String           -- Description
                                 (Maybe Difficulty)
                                 (Maybe ID)       -- Character
                                 [ParameterInfo]
                                 [Toggle]
                                 
tScenarioInfo :: Type a ScenarioInfo
tScenarioInfo = 
    Iso ((<-!) pairify) (Pair (Tag "name"            tString)
                        (Pair (Tag "description"     tString)
                        (Pair                       (tMaybe tDifficulty)
                        (Pair (Tag "character"      (tMaybe tString))
                        (Pair (Tag "parameters"     (tList tParameterInfo))
                              (Tag "toggles"        (tList tToggle)))))))
      where 
        pairify (ScenarioInfo name descr diff char ps ts) =
            (name, (descr, (diff, (char, (ps, ts)))))

data ParameterInfo = ParameterInfo ID
                                   Name
                                   String -- Description

tParameterInfo :: Type a ParameterInfo
tParameterInfo = Iso ((<-!) pairify) (Pair (Tag "id"            tString)
                                     (Pair (Tag "name"          tString)
                                           (Tag "description"   tString)))
        where pairify (ParameterInfo pid name descr) = (pid, (name, descr))

tToggle :: Type a Toggle
tToggle = Iso ((<-!) pairify) (Pair (Tag "name" tString) 
                                    (Tag "bool" tBool)) 
  where pairify (Toggle name boolean) = (name, boolean)

-- ScoreResult type -------------------------------------------------------------------------------------------------------------

data ScoreResult = ScoreResult Score           -- Total score as a percentage
                               [SubScore]      -- All subscores for all scored parameters

tScoreResult :: Type a ScoreResult
tScoreResult =
    Iso ((<-!) pairify) (Pair (Tag "mainscore"          tInt)
                              (Tag "subscores"         (tList (tTuple3 tString tString tInt))))
        where pairify (ScoreResult score subscores) = (score, subscores)
