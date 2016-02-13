{-# LANGUAGE DeriveGeneric #-}
------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Domain.Scenarios.Globals where

import Data.Maybe

import GHC.Generics
import Data.Binary

-- | Type definitions 
type ParameterValue = Int
type Emotion = String
type ID = String
type Name = String
type Score = Int

data StatementInfo = StatementInfo 
    { statType     :: StatementType  
    , statText     :: StatementText
    , statIntents  :: [String]       -- intentions
    , statFeedback :: (Maybe String) -- feedback
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
    
    {-
instance Show Toggle where
    show (Toggle name boolean) = show name ++ ": " ++ show boolean ++ "\n"
    -}
--------------------------------------------------------------------------------------------------------------------------
    
-- Specifies a parameter that can be an emotion or a goal of a conversation like "inleven"
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

{-
instance Show Parameter where
    show (Parameter id name initvalue descr scored max min) = 
        show id    ++ "\t" ++ show name ++ "\t" ++ show initvalue ++ "\t" ++ 
        show descr ++ "\t" ++ show max  ++ "\t" ++ show min       ++ "\t" ++ 
        show scored ++ "\n"     -}

-- | Extra functions for getting a type out of Monad to catch the fail case, 
-- | which needs to be done when calling findAttribute and findChild from the Ideas XML interface.
errorOnFail :: String -> Maybe a -> a
errorOnFail errorMsg = fromMaybe (error errorMsg) 

emptyOnFail :: Maybe [a] -> [a]
emptyOnFail = fromMaybe []

-- | Applies a function to the first element of a list, if there is one.
applyToFirst :: (a -> a) -> [a] -> [a]
applyToFirst _ []     = []
applyToFirst f (x:xs) = f x : xs