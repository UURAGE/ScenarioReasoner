{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

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
                                 [ParameterInfo]

tScenarioInfo :: Type a ScenarioInfo
tScenarioInfo =
    Iso ((<-!) pairify) (Pair (Tag "name"            tString)
                        (Pair (Tag "description"     tString)
                        (Pair                       (tMaybe tDifficulty)
                              (Tag "parameters"     (tList tParameterInfo)))))
      where
        pairify (ScenarioInfo name descr diff ps) =
            (name, (descr, (diff, ps)))

data ParameterInfo = ParameterInfo ID
                                   Name
                                   String -- Description

tParameterInfo :: Type a ParameterInfo
tParameterInfo = Iso ((<-!) pairify) (Pair (Tag "id"            tString)
                                     (Pair (Tag "name"          tString)
                                           (Tag "description"   tString)))
        where pairify (ParameterInfo pid name descr) = (pid, (name, descr))

-- ScoreResult type -------------------------------------------------------------------------------------------------------------

data ScoreResult = ScoreResult Score           -- Total score as a percentage
                               [SubScore]      -- All subscores for all scored parameters

tScoreResult :: Type a ScoreResult
tScoreResult =
    Iso ((<-!) pairify) (Pair (Tag "mainscore"          tInt)
                              (Tag "subscores"         (tList (tTuple3 tString tString tInt))))
        where pairify (ScoreResult score subscores) = (score, subscores)
