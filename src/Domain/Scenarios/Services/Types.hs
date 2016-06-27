{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Services.Types where

import Ideas.Common.Library
import Ideas.Encoding.Encoder
import Ideas.Service.Types
import Ideas.Text.JSON

import Domain.Scenarios.Globals
import Domain.Scenarios.ScenarioState()

-- | Ideas Framework type definitions for sending an object through a service

-- ScenarioInfo types -----------------------------------------------------------------------------------------------------------

data ScenarioInfo = ScenarioInfo Name
                                 String           -- Description
                                 (Maybe Difficulty)
                                 [ParameterInfo]
                                 PropertyValues

tScenarioInfo :: Type a ScenarioInfo
tScenarioInfo =
    Iso ((<-!) pairify) (Pair         (Tag "name"           tString)
                        (Pair         (Tag "description"    tString)
                        (Pair (tMaybe (tag "difficulty"     tDifficulty))
                        (Pair         (Tag "parameters"     (tList tParameterInfo))
                                      (Tag "propertyValues" tPropertyValues)))))
      where
        pairify (ScenarioInfo name descr diff ps pvs) =
            (name, (descr, (diff, (ps, pvs))))
        tag s (Tag _ t) = Tag s t
        tag s t         = Tag s t

data ParameterInfo = ParameterInfo ID
                                   Name
                                   String -- Description

tParameterInfo :: Type a ParameterInfo
tParameterInfo = Iso ((<-!) pairify) (Pair (Tag "id"            tString)
                                     (Pair (Tag "name"          tString)
                                           (Tag "description"   tString)))
        where pairify (ParameterInfo pid name descr) = (pid, (name, descr))

tPropertyValues :: Type a PropertyValues
tPropertyValues = tCharactered (assocsToTerm (jsonToTerm . toJSON))

tCharactered :: (b -> Term) -> Type a (Charactered b)
tCharactered subToTerm = Iso ((<-!) pairify) (Pair (Tag "independent" tTerm) (Tag "perCharacter" tTerm))
        where
          pairify (Charactered ivs pcvs) = (subToTerm ivs, assocsToTerm subToTerm pcvs)

assocsToTerm :: (a -> Term) -> Assocs a -> Term
assocsToTerm subToTerm (Assocs vs) = TCon (newSymbol "object") (concatMap toKVP vs)
        where
          toKVP (key, value) = [TVar key, subToTerm value]
