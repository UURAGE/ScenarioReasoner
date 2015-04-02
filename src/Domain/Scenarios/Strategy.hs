module Domain.Scenarios.Strategy where

import GHC.Exts(sortWith)

import Data.Maybe

import Control.Monad hiding (sequence)
import Data.List
import qualified Data.Map as M

import Ideas.Common.Library
import Ideas.Common.Strategy.Combinators hiding (not)

import Domain.Scenarios.Types
import Domain.Scenarios.Parser
import Ideas.Text.XML.Interface(Element)

--Framework code, try not to break your head.
guardedRule :: IsId b => b -> String -> (EmotionalState -> Bool) -> (EmotionalState -> EmotionalState) -> Rule EmotionalState
guardedRule identifier description precondCheck applyEffects =
    describe description $ makeRule identifier (\state -> do guard $ precondCheck state; Just $ applyEffects state)
    --Make description and add it to the rule and make the rule if the precondition holds
   
makeGuardedRule :: ID -> Statement -> Tree -> String -> Rule EmotionalState
makeGuardedRule scriptID statement tree interleaved = guardedRule
    (["scenarios", scriptID, toIdTypeSegment (statType statement), statID statement, interleaved]) -- create an identifier for the rule
    (either id (intercalate " // " . map snd) (statDescription statement))                         -- make a description of the rule
    (evaluateMaybeCondition (statPrecondition statement))                                          -- check if precondition is fulfilled
    (\state -> foldr applyEffect (fst state, treeID tree) (statEffects statement))                 -- apply the effects of a statement to the emotional state
    --The initial state is not generated here. 
    --It is generated at exercises.hs then the frontend requests it with the "examples" method and sends it back with the first "allfirsts" request
            

-- Takes the dialogue, sorts it with the level of interleaving and makes a strategy foreach level          
makeStrategy :: Monad m => Script -> m (Strategy EmotionalState)
makeStrategy script = do
    dialogue <- parseDialogue script
    let sortedDialogue = sortWith (\(level, _) -> level) dialogue
    scriptID <- getScriptId script
    intLvlStrategies <- mapM (\intLvl -> makeIntLvlStrategy intLvl scriptID) sortedDialogue
    return (sequence' intLvlStrategies)
      where 
        sequence' = Ideas.Common.Strategy.Combinators.sequence
        
-- Foreach tree (subject) in an interleave level make a strategy 
makeIntLvlStrategy :: Monad m => InterleaveLevel -> ID -> m (Strategy EmotionalState) 
makeIntLvlStrategy (_, trees) scriptID = do
    treeStrategies <- mapM (\tree -> makeTreeStrategy tree scriptID) trees
    return (interleave treeStrategies)
    
-- Recursively make a strategy for a tree by walking down the whole tree
makeTreeStrategy :: Monad m => Tree -> ID -> m (Strategy EmotionalState)
makeTreeStrategy tree scriptID = do
    let startID = treeStartID tree    
    strategy <- makeStatementStrategy tree scriptID startID    
    return strategy

makeStatementStrategy :: Monad m => Tree -> ID -> ID -> m (Strategy EmotionalState)
makeStatementStrategy tree scriptID statementID = do 
    -- Find the given statement in the tree with the id
    let statement = errorOnFail $ find (\stat -> statID stat == statementID) (treeStatements tree)
    
    -- Make a rule for the statement and tell the framework if we want to interleave here or not. 
    -- (processed in Ideas.Common.Strategy.Parsing.hs in the switch function)
    let rule | jumpPoint statement = makeGuardedRule scriptID statement tree "interleaved"
             | otherwise           = makeGuardedRule scriptID statement tree "" 
                        
    let nextIDs = nextStatIDs statement
    
    case nextIDs of 
        [] -> return (toStrategy rule)
        _  -> do
            -- Foreach next statement make a strategy
            nextStrategyList <- mapM (makeStatementStrategy tree scriptID) nextIDs
            
            let nextStrategy = alternatives nextStrategyList
            
            let strategy = rule <*> nextStrategy
                         
            return strategy