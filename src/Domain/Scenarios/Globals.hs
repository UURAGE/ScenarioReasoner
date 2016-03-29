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
    , statFeedback :: (Maybe String)
    , statMedia    :: MediaInfo
    , statEnd      :: Bool
    }
    deriving (Show, Eq, Read, Generic)

instance Binary StatementInfo

-- The text of a statement is either simply a text or a conversation with a list of tuples of the types and texts
type StatementType = String                                         -- Conversation / Player / Computer
type StatementText = Either String [(ConversationTextType, String)] -- Either Text [(Type, Text)]
type ConversationTextType = String                                  -- Player / Computer / Situation

-- MediaInfo [(VisualType, VisualID)] [AudioID] where VisualType is either an "image" or a "video"
data MediaInfo = MediaInfo [(String, ID)] [ID] 
    deriving(Show, Eq, Read, Generic)
    
instance Binary MediaInfo
    
emptyStatementInfo :: StatementInfo
emptyStatementInfo = StatementInfo "" (Left "") [] Nothing (MediaInfo [] []) False
                
-- Specifies if a certain feature should be on or off
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

-- | Extra functions for getting a type out of Monad to catch the fail case, 
-- | which needs to be done when calling findAttribute and findChild from the Ideas XML interface
errorOnFail :: String -> Maybe a -> a
errorOnFail errorMsg = fromMaybe (error errorMsg) 

emptyOnFail :: Maybe [a] -> [a]
emptyOnFail = fromMaybe []

-- | Applies a function to the first element of a list, if there is one
applyToFirst :: (a -> a) -> [a] -> [a]
applyToFirst _ []     = []
applyToFirst f (x:xs) = f x : xs
