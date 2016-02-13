{-# LANGUAGE DeriveGeneric #-}
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
    deriving (Show,Generic )
    
instance Binary Scenario

instance HasId Scenario where
    getId (Scenario metadata _ _) = either error id $ do
                let name = scenarioName metadata
                let descr = scenarioDescription metadata
                return $ describe descr $ "scenarios" # name
    changeId _ _ = error "The ID of a Script is determined externally."

data MetaData = MetaData    
        { scenarioName            :: Name
        , scenarioDescription     :: String
        , scenarioDifficulty      :: Maybe Difficulty
        , scenarioCharacter       :: Maybe ID
        , scenarioParameters      :: [Parameter]
        , scenarioToggles         :: [Toggle]
        , scenarioScoringFunction :: ScoringFunction
        }      
 deriving Generic
        
instance Binary MetaData
instance Binary Difficulty where
   get = do () <- get; return Medium
   put _ = put ()

instance Show MetaData where 
    show (MetaData name desc diff char ps ts sf) =
        "scenario name: "   ++ show name  ++ "\n" ++
        "description: "     ++ show desc  ++ "\n" ++
        "difficulty: "      ++ show diff  ++ "\n" ++
        "character: "       ++ show char  ++ "\n" ++
        "parameters: "      ++ show ps    ++ "\n" ++
        "toggles: "         ++ show ts    ++ "\n" ++
        "scoringFunction: " ++ show sf    ++ "\n"
        
type FeedbackForm = [FeedbackFormEntry]

data FeedbackFormEntry = FeedbackFormEntry
    { feedbackParamID    :: ID
    , feedbackConditions :: [(Condition, String)]
    , feedbackDefault    :: Maybe String
    }
 deriving Generic

instance Binary FeedbackFormEntry


instance Show FeedbackFormEntry where
    show (FeedbackFormEntry pid cond def) = show pid ++ "\t" ++ show cond ++ "\t" ++ show def ++ "\n"

      
type Dialogue = [InterleaveLevel]

type InterleaveLevel = (Int, [Tree])

data Tree = Tree
        { treeID          :: ID
        , treeStartIDs    :: [ID]
        , treeAtomic      :: Bool 
        , treeOptional    :: Bool
        , treeStatements  :: [Statement]
        }       
 deriving Generic

instance Binary Tree

instance Show Tree where
    show (Tree tid start atom opt stats) = "\n" ++
        "tree: "        ++ show tid   ++ 
        " start: "      ++ show start ++
        " atomic: "     ++ show atom  ++
        " optional: "   ++ show opt   ++
        " statements: " ++ show stats ++ "\n"      
        
data Statement = Statement
        { statID            :: ID
        , statInfo          :: StatementInfo
        , statPrecondition  :: Maybe Condition
        , statParamEffects  :: [Effect]
        , jumpPoint         :: Bool
        , statInits         :: Bool
        , nextStatIDs       :: [ID]
        }
 deriving Generic

instance Binary Statement

instance Show Statement where
    show (Statement sid info pc pes jp ini nexts) = "\n  " ++
        "statement: "     ++ show sid   ++ "\n\t" ++ 
        " info: "         ++ show info  ++ "\n\t" ++
        " precondition: " ++ show pc    ++ "\n\t" ++
        " paramEffects: " ++ show pes   ++ "\n\t" ++
        " jumpPoint: "    ++ show jp    ++ "\n\t" ++
        " inits: "        ++ show ini   ++ "\n\t" ++
        " nextIDs: "      ++ show nexts ++ "\n\t" 
        
instance HasId Statement where
    getId statement = either error id $ do
                let statementID = statID statement
                let statementText = statText (statInfo statement)
                let statementDescription = either id (intercalate " // " . map snd) statementText
                return $ describe statementDescription $ newId statementID
    changeId _ _ = error "The ID of a Statement is determined externally."  
    