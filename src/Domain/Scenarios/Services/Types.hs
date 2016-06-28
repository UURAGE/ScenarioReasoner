{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Services.Types where

import qualified Data.Map as M

import Ideas.Common.Library
import Ideas.Encoding.Encoder
import Ideas.Service.Types
import Ideas.Text.JSON

import qualified Domain.Scenarios.DomainData as DD
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
    Iso ((<-!) pairify) (Pair         (Tag "name"                  tString)
                        (Pair         (Tag "description"           tString)
                        (Pair (tMaybe (tag "difficulty"            tDifficulty))
                        (Pair         (Tag "userDefinedParameters" (tList tParameterInfo))
                                      (Tag "propertyValues"        tPropertyValues)))))
      where
        pairify (ScenarioInfo name descr diff ps pvs) =
            (name, (descr, (diff, (ps, pvs))))
        tag s (Tag _ t) = Tag s t
        tag s t         = Tag s t

data ParameterInfo = ParameterInfo ID
                                   Name
                                   (Maybe String) -- Description
                                   DD.Type

tParameterInfo :: Type a ParameterInfo
tParameterInfo = Iso ((<-!) pairify) (Pair (Tag "id"            tString)
                                     (Pair (Tag "name"          tString)
                                     (Pair (Tag "description"   (tMaybe tString))
                                           (Tag "type"          tDomainDataType))))
        where pairify (ParameterInfo pid name descr ty) = (pid, (name, (descr, ty)))

tDomainDataType :: Type a DD.Type
tDomainDataType = Iso ((<-!) (jsonToTerm . toJSON)) tTerm

tPropertyValues :: Type a PropertyValues
tPropertyValues = tCharactered (assocsToTerm (jsonToTerm . toJSON))

tCharactered :: (b -> Term) -> Type a (Charactered b)
tCharactered subToTerm = Iso ((<-!) pairify) (Pair (Tag "independent" tTerm) (Tag "perCharacter" tTerm))
        where
          pairify (Charactered ivs pcvs) = (subToTerm ivs, stringMapToTerm subToTerm pcvs)

assocsToTerm :: (a -> Term) -> Assocs a -> Term
assocsToTerm subToTerm (Assocs vs) = TCon (newSymbol "object") (concatMap toKVP vs)
        where
          toKVP (key, value) = [TVar key, subToTerm value]

stringMapToTerm :: (a -> Term) -> M.Map String a -> Term
stringMapToTerm subToTerm m = TCon (newSymbol "object") (concatMap toKVP (M.toList m))
        where
          toKVP (key, value) = [TVar key, subToTerm value]
