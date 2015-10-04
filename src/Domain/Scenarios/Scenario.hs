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

data Scenario = Scenario 
        { scenarioMetaData     :: MetaData
        , scenarioFeedbackForm :: FeedbackForm
        , scenarioDialogue     :: Dialogue  
        }
    deriving (Show,Generic )
    
instance Binary Scenario

instance HasId Scenario where
    getId (Scenario metadata _ _) = either error id $ do
                let id = scenarioID metadata
                let descr = scenarioDescription metadata
                return $ describe descr $ "scenarios" # id
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
 deriving Generic
        
instance Binary MetaData
instance Binary Difficulty where
   get = do () <- get; return Medium
   put _ = put ()
{- TODO
instance Binary Difficulty where
    get = do t <- get :: Get String
             case t of
                  "diff" -> do diff <- get
                               return (errorOnFail "no difficulty parse"$ readDifficulty diff)
                  _      -> error "no binary difficulty"
                       
    put difficulty = do put ("diff" :: String)
                        put (show difficulty)-}

instance Show MetaData where 
    show (MetaData id name desc diff bi ci model emo ps loc pet ts sf se) =
        "id: " ++ show id   ++ "  name: " ++ show name ++ "\n" ++ 
        "description: "     ++ show desc  ++ "\n" ++ 
        "difficulty: "      ++ show diff  ++ "\n" ++
        "bannerImage: "     ++ show bi    ++ "\n" ++ 
        "characterImage: "  ++ show ci    ++ "\n" ++ 
        "model: "           ++ show model ++ "\n" ++ 
        "startEmotion: "    ++ show emo   ++ "\n" ++
        "parameters: "      ++ show ps    ++ "\n" ++ 
        "location: "        ++ show loc   ++ "\n" ++
        "pet: "             ++ show pet   ++ "\n" ++
        "toggles: "         ++ show ts    ++ "\n" ++ 
        "scoringFunction: " ++ show sf    ++ "\n" ++ 
        "scoreExtremes: "   ++ show se    ++ "\n"  
        
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
    show (Tree id start atom opt stats) = "\n" ++
        "tree: "        ++ show id    ++ 
        " start: "      ++ show start ++
        " atomic: "     ++ show atom  ++
        " optional: "   ++ show opt   ++
        " statements: " ++ show stats ++ "\n"      
        
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
 deriving Generic

instance Binary Statement

instance Show Statement where
    show (Statement id info pc pes ees jp inits nexts) = "\n  " ++
        "statement: "     ++ show id    ++ "\n\t" ++ 
        " info: "         ++ show info  ++ "\n\t" ++
        " precondition: " ++ show pc    ++ "\n\t" ++
        " paramEffects: " ++ show pes   ++ "\n\t" ++
        " emotionEffects: " ++ show ees ++ "\n\t" ++
        " jumpPoint: "    ++ show jp    ++ "\n\t" ++
        " inits: "        ++ show inits ++ "\n\t" ++
        " nextIDs: "      ++ show nexts ++ "\n\t" 
        
instance HasId Statement where
    getId statement = either error id $ do
                let statementId = statID statement
                let statementText = statText (statInfo statement)
                let statementDescription = either id (intercalate " // " . map snd) statementText
                return $ describe statementDescription $ newId statementId
    changeId _ _ = error "The ID of a Statement is determined externally."  
    