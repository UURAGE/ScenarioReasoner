{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
module Domain.Scenarios.Globals where

import Ideas.Text.XML.Interface(Element)

-- | Type definitions 
type ScriptElement = Element
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
    , parameterInitialValue :: ParameterValue
    , parameterScored       :: Bool
    }