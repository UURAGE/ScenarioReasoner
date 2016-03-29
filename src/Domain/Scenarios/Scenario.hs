{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.Scenario where

import Data.List(intercalate)

import Ideas.Common.Library

import Domain.Scenarios.Globals
import Domain.Scenarios.ScoringFunction
import Domain.Scenarios.Condition
import Domain.Scenarios.ScenarioState(Effect)
import GHC.Generics
import Data.Binary

import System.IO.Unsafe

readBinaryScenario :: FilePath -> Scenario
readBinaryScenario path = unsafePerformIO $ decodeFile path

data Scenario = Scenario 
        { scenarioMetaData     :: MetaData
        , scenarioFeedbackForm :: FeedbackForm
        , scenarioDialogue     :: Dialogue  
        }
    deriving (Show, Read, Generic)
    
instance Binary Scenario


data MetaData = MetaData    
        { scenarioName            :: Name
        , scenarioDescription     :: String
        , scenarioDifficulty      :: Maybe Difficulty
        , scenarioCharacter       :: Maybe ID
        , scenarioParameters      :: [Parameter]
        , scenarioToggles         :: [Toggle]
        , scenarioScoringFunction :: ScoringFunction
        }      
 deriving (Show, Read, Generic)
        
instance Binary MetaData

deriving instance Generic Difficulty
instance Binary Difficulty

type FeedbackForm = [FeedbackFormEntry]

data FeedbackFormEntry = FeedbackFormEntry
    { feedbackParamID    :: ID
    , feedbackConditions :: [(Condition, String)]
    , feedbackDefault    :: Maybe String
    }
 deriving (Show, Read, Generic)

instance Binary FeedbackFormEntry

type Dialogue = [InterleaveLevel]

type InterleaveLevel = [Tree]

data Tree = Tree
        { treeID          :: ID
        , treeStartIDs    :: [ID]
        , treeAtomic      :: Bool 
        , treeOptional    :: Bool
        , treeStatements  :: [Statement]
        }       
 deriving (Show, Read, Generic)

instance Binary Tree

data Statement = Statement
        { statID            :: ID
        , statInfo          :: StatementInfo
        , statPrecondition  :: Maybe Condition
        , statParamEffects  :: [Effect]
        , jumpPoint         :: Bool
        , statInits         :: Bool
        , nextStatIDs       :: [ID]
        }
 deriving (Show, Read, Generic)

instance Binary Statement
