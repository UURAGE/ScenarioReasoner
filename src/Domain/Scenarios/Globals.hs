{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
module Domain.Scenarios.Globals where

import Data.Maybe
import Data.Char(isLower, toLower)

import Ideas.Text.JSON
import Ideas.Text.XML.Interface(Element)

-- | Type definitions 
type Script = Element
type ParameterValue = Int
type Emotion = String
type ID = String
type Name = String
type Score = Int

-- Global datastructures and utility functions ---------------------------------------------------------------------------

-- Specifies if a certain feature should be on or off
data Toggle = Toggle Name Bool
    
instance Show Toggle where
    show (Toggle name boolean) = show name ++ ": " ++ show boolean ++ "\n"
    
toggleNames :: [Name]
toggleNames = ["showscore"    --score at the end of the game
              ,"showfeedback" --feedback at the end of the game
              , "feedback"]   --feedback during the game
              
-- StatementInfo ---------------------------------------------------------------------------------------------------------
data StatementInfo = StatementInfo 
    { statType     :: StatementType  
    , statText     :: StatementText
    , statIntents  :: [String]       -- intentions
    , statFeedback :: (Maybe String) -- feedback
    , statMedia    :: MediaInfo
    , statEnd      :: Bool
    }
    deriving (Show, Eq)

-- The text of a statement is either simply a text or a conversation with a list of tuples of the types and texts 
type StatementType = String                                         -- Conversation / Player / Computer
type StatementText = Either String [(ConversationTextType, String)] -- Either Text [(Type, Text)] 
type ConversationTextType = String                                  -- Player / Computer / Situation

-- MediaInfo [(VisualType, VisualID)] AudioIDs where VisualType is either an "image" or a "video" 
data MediaInfo = MediaInfo [(String, ID)] [ID] 
    deriving(Show, Eq)
--------------------------------------------------------------------------------------------------------------------------
    
-- Specifies a parameter that can be an emotion or a goal of a conversation like "inleven"
data Parameter = Parameter
    { parameterId           :: ID
    , parameterName         :: Name
    , parameterInitialValue :: Maybe ParameterValue
    , parameterScored       :: Bool
    }
    
instance Show Parameter where
    show (Parameter id name initvalue scored) = 
        show id ++ "\t" ++ show name ++ "\t" ++ "\t" ++ show initvalue ++ "\t" ++ show scored ++ "\n"
    
-- | Returns the initial value of a parameter, or zero if it does not have one.
parameterInitialValueOrZero :: Parameter -> ParameterValue
parameterInitialValueOrZero = fromMaybe 0 . parameterInitialValue
           
-- | Extra functions for getting a type out of Monad to catch the fail case, 
-- | which needs to be done when calling findAttribute and findChild from the Ideas XML interface.
errorOnFail :: String -> Maybe a -> a
errorOnFail errorMsg ma = fromMaybe (error errorMsg) ma

emptyOnFail :: Maybe [a] -> [a]
emptyOnFail = fromMaybe []

nothingOnFail :: Maybe a -> Maybe a
nothingOnFail (Just a) = Just a
nothingOnFail _        = Nothing

-- | Applies a function to the first element of a list, if there is one.
applyToFirst :: (a -> a) -> [a] -> [a]
applyToFirst _ []     = []
applyToFirst f (x:xs) = (f x) : xs