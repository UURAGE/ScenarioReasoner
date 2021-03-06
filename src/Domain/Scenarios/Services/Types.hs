{- © Utrecht University (Department of Information and Computing Sciences) -}

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

data ScenarioInfo = ScenarioInfo Id
                                 Name
                                 (Maybe String)   -- Language
                                 String           -- Description
                                 (Maybe Difficulty)
                                 (Maybe Int)      -- Version
                                 [CharacterDefinitionInfo]
                                 [DefinitionInfo] -- Expressions
                                 [DefinitionInfo] -- User-defined parameters
                                 PropertyValues

tScenarioInfo :: Type a ScenarioInfo
tScenarioInfo =
    Iso ((<-!) pairify) (Pair         (Tag "id"                    tReadShow)
                        (Pair         (Tag "name"                  tString)
                        (Pair (tMaybe (Tag "language"              tString))
                        (Pair         (Tag "description"           tString)
                        (Pair (tMaybe (tag "difficulty"            tDifficulty))
                        (Pair (tMaybe (Tag "version"               tInt))
                        (Pair         (Tag "characters"            (tList tCharacterDefinitionInfo))
                        (Pair         (Tag "expressions"           (tList tDefinitionInfo))
                        (Pair         (Tag "userDefinedParameters" (tList tDefinitionInfo))
                                      (Tag "propertyValues"        tPropertyValues))))))))))
      where
        pairify (ScenarioInfo sid name lang descr diff ver cs es ps pvs) =
            (sid, (name, (lang, (descr, (diff, (ver, (cs, (es, (ps, pvs)))))))))
        tag s (Tag _ t) = Tag s t
        tag s t         = Tag s t

tReadShow :: (Show b, Read b) => Type a b
tReadShow = Iso (read <-> show) tString

data CharacterDefinitionInfo = CharacterDefinitionInfo ID
                                                       (Maybe Name)

tCharacterDefinitionInfo :: Type a CharacterDefinitionInfo
tCharacterDefinitionInfo = Iso ((<-!) pairify) (Pair         (Tag "id"   tString)
                                                     (tMaybe (Tag "name" tString)))
        where pairify (CharacterDefinitionInfo pid name) = (pid, name)

data DefinitionInfo = DefinitionInfo ID
                                     Name
                                     (Maybe String) -- Description
                                     DD.Type

tDefinitionInfo :: Type a DefinitionInfo
tDefinitionInfo = Iso ((<-!) pairify) (Pair         (Tag "id"            tString)
                                      (Pair         (Tag "name"          tString)
                                      (Pair (tMaybe (Tag "description"   tString))
                                                    (Tag "type"          tDomainDataType))))
        where pairify (DefinitionInfo pid name descr ty) = (pid, (name, (descr, ty)))

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

tValueAssocs :: Type a (Assocs DD.Value)
tValueAssocs = Iso ((<-!) (assocsToTerm (jsonToTerm . (\j -> Object [("value", j)]) . toJSON))) tTerm

tDomainDataValue :: Type a DD.Value
tDomainDataValue = Iso ((<-!) (jsonToTerm . toJSON)) tTerm
