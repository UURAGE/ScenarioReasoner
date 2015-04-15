module Domain.Scenarios.Globals where

import Data.Maybe

import Ideas.Text.XML.Interface(Element)

-- | Type definitions 
type ScriptElement = Element
type TreeElem = Element
type StatElem = Element
type ParameterValue = Int
type Emotion = String
type ID = String
type Name = String
type Score = Int

-- global datastructures

data Toggle = Toggle Name Bool
    deriving (Show)
    
toggleNames :: [Name]
toggleNames = ["showscore"    --score at the end of the game
              ,"showfeedback" --feedback at the end of the game
              , "feedback"]   --feedback during the game
    
-- specifies a parameter that can be an emotion or a goal of a conversation like "structureren"
data Parameter = Parameter
    { parameterId           :: ID
    , parameterName         :: Name
    , parameterEmotion      :: Maybe Emotion
    , parameterInitialValue :: Maybe ParameterValue
    , parameterScored       :: Bool
    }
    deriving(Show)
    
-- | Returns the initial value of a parameter, or zero if it does not have one.
parameterInitialValueOrZero :: Parameter -> ParameterValue
parameterInitialValueOrZero = fromMaybe 0 . parameterInitialValue
                      


-- | Extra functions for getting a type out of Monad
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