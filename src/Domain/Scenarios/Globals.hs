{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable #-}
{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Globals where

import GHC.Generics

import Data.Binary
import qualified Data.Map as M
import Data.Maybe

import qualified Domain.Scenarios.DomainData as DD

type ID = String
type Name = String

data StatementInfo = StatementInfo
    { statType           :: StatementType
    , statCharacterIdref :: Maybe ID
    , statText           :: StatementText
    , statPropertyValues :: PropertyValues
    }
    deriving (Show, Eq, Read, Generic)

instance Binary StatementInfo

type StatementType = String
type StatementText = String

type PropertyValues = Charactered (Assocs DD.Value)

data Charactered a = Charactered
    { characteredIndependent  :: a
    , characteredPerCharacter :: M.Map String a
    }
    deriving (Show, Eq, Read, Functor, Foldable, Generic)

instance Binary a => Binary (Charactered a)

data Assocs a = Assocs [(String, a)]
    deriving (Show, Read, Eq, Generic)

instance Binary a => Binary (Assocs a)

--------------------------------------------------------------------------------------------------------------------------

data Definitions = Definitions
    { definitionsCharacters :: [CharacterDefinition]
    , definitionsProperties :: ([Definition], TypeMap)
    , definitionsParameters :: (Usered [Definition], TypeMap)
    }
 deriving (Show, Read, Generic)

instance Binary Definitions

data CharacterDefinition = CharacterDefinition
    { characterDefinitionId   :: ID
    , characterDefinitionName :: Maybe Name
    }
 deriving (Show, Read, Generic)

instance Binary CharacterDefinition

type TypeMap = M.Map ID DD.Type

data Usered a = Usered
    { useredUserDefined :: a
    , useredFixed       :: a
    }
 deriving (Show, Read, Functor, Foldable, Generic)

instance Binary a => Binary (Usered a)

data Definition = Definition
    { definitionId           :: ID
    , definitionName         :: Name
    , definitionDescription  :: Maybe String
    , definitionType         :: DD.Type
    , definitionDefault      :: Maybe DD.Value
    }
 deriving (Show, Read, Generic)

instance Binary Definition

type ParameterState = Usered (Charactered ParameterMap)

type ParameterMap = M.Map ID DD.Value

-- Functions for dealing with the Nothing case of a Maybe value produced by the
-- implementation of fail in the Monad instance of Maybe.
-- Useful for handling failure of findAttribute and findChild (from Ideas.Text.XML.Interface).

errorOnFail :: String -> Maybe a -> a
errorOnFail errorMsg = fromMaybe (error errorMsg)

emptyOnFail :: Maybe [a] -> [a]
emptyOnFail = fromMaybe []

-- | Applies a function to the first element of a list, if there is one
applyToFirst :: (a -> a) -> [a] -> [a]
applyToFirst _ []     = []
applyToFirst f (x:xs) = f x : xs
