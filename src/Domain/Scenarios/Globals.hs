{-# LANGUAGE DeriveGeneric #-}
{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Globals where

import GHC.Generics

import Data.Binary
import Data.Maybe

type ParameterValue = Int
type ID = String
type Name = String
type Score = Int

data StatementInfo = StatementInfo
    { statType     :: StatementType
    , statText     :: StatementText
    , statIntents  :: [String]
    , statFeedback :: Maybe String
    , statMedia    :: MediaInfo
    }
    deriving (Show, Eq, Read, Generic)

instance Binary StatementInfo

type StatementType = String                                         -- conversation / player / computer
-- The text of a statement is either simply a text or a conversation with a list of tuples of the types and texts
type StatementText = Either String [(ConversationTextType, String)] -- Either Text [(Type, Text)]
type ConversationTextType = String                                  -- player / computer / situation

data MediaInfo = MediaInfo
    { mediaVisuals :: [(VisualType, ID)]
    , mediaAudios  :: [ID]
    }
    deriving (Show, Eq, Read, Generic)

type VisualType = String -- image / video

instance Binary MediaInfo

-- | Specifies if a certain feature should be on or off
data Toggle = Toggle Name Bool
    deriving (Show, Read, Generic)

instance Binary Toggle

--------------------------------------------------------------------------------------------------------------------------

data Parameter = Parameter
    { parameterId           :: ID
    , parameterName         :: Name
    , parameterInitialValue :: Maybe ParameterValue
    , parameterDescription  :: String
    , parameterScored       :: Bool
    , parameterMax          :: Maybe ParameterValue
    , parameterMin          :: Maybe ParameterValue
    }
 deriving (Show, Read,Generic)

instance Binary Parameter

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
