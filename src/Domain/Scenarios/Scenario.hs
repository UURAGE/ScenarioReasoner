{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

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

instance HasId Scenario where
    getId (Scenario metadata _ _) = either error id $ do
                let sID = scenarioID metadata
                let descr = scenarioDescription metadata
                return $ describe descr $ "scenarios" # sID
    changeId _ _ = error "The ID of a Script is determined externally."

data MetaData = MetaData    
        { scenarioID              :: ID
        , scenarioName            :: Name
        , scenarioDescription     :: String
        , scenarioDifficulty      :: Difficulty 
        , scenarioBannerImage     :: Maybe ID
        , scenarioCharacterImage  :: Maybe ID
        , scenarioModel           :: Maybe ID 
        , scenarioStartEmotion    :: Maybe Emotion
        , scenarioParameters      :: [Parameter]
        , scenarioLocation        :: Name
        , scenarioPet             :: Name
        , scenarioToggles         :: [Toggle]
        , scenarioScoringFunction :: ScoringFunction
        , scenarioScoreExtremes   :: Maybe (Score, Score)
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
        , statEmotionEffects:: [Effect]
        , jumpPoint         :: Bool
        , statInits         :: Bool
        , nextStatIDs       :: [ID]
        }
 deriving (Show, Read, Generic)

instance Binary Statement

instance HasId Statement where
    getId statement = either error id $ do
                let statementID = statID statement
                let statementText = statText (statInfo statement)
                let statementDescription = either id (intercalate " // " . map snd) statementText
                return $ describe statementDescription $ newId statementID
    changeId _ _ = error "The ID of a Statement is determined externally."  
