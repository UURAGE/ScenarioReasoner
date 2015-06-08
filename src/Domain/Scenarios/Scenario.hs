module Domain.Scenarios.Scenario where

import Data.List(intercalate)

import Ideas.Common.Library
import Ideas.Text.XML

import Domain.Scenarios.Globals
import Domain.Scenarios.ScoringFunction
import Domain.Scenarios.Condition
import Domain.Scenarios.ScenarioState(Effect)

data Scenario = Scenario 
        { scenarioMetaData :: MetaData
        , scenarioDialogue :: Dialogue        
        }
    deriving(Show)
    
instance HasId Scenario where
    getId (Scenario metadata _) = either error id $ do
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
        , scenarioParameters      :: [Parameter]
        , scenarioLocation        :: Name
        , scenarioToggles         :: [Toggle]
        , scenarioScoringFunction :: ScoringFunction
        , scenarioScoreExtremes   :: Maybe (Score, Score)
        , scenarioFeedbackForm    :: FeedbackForm
        }
        
instance Show MetaData where 
    show (MetaData id name desc diff bi ci model ps loc ts sf se fbf) =
        "id: " ++ show id   ++ "  name: " ++ show name ++ "\n" ++ 
        "description: "     ++ show desc  ++ "\n" ++ 
        "difficulty: "      ++ show diff  ++ "\n" ++
        "bannerImage: "     ++ show bi    ++ "\n" ++ 
        "characterImage: "  ++ show ci    ++ "\n" ++ 
        "model: "           ++ show model ++ "\n" ++ 
        "parameters: "      ++ show ps    ++ "\n" ++ 
        "location: "        ++ show loc   ++ "\n" ++
        "toggles: "         ++ show ts    ++ "\n" ++ 
        "scoringFunction: " ++ show sf    ++ "\n" ++ 
        "scoreExtremes: "   ++ show se    ++ "\n" ++
        "feedbackForm: "    ++ show fbf   ++ "\n"
        
type FeedbackForm = [FeedbackFormEntry]

data FeedbackFormEntry = FeedbackFormEntry
    { feedbackParamID    :: ID
    , feedbackConditions :: [(Condition, String)]
    , feedbackDefault    :: Maybe String
    }
    
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
    