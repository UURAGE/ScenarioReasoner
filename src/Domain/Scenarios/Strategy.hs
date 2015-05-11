module Domain.Scenarios.Strategy where

import GHC.Exts(sortWith)

import Data.Maybe

import Control.Monad hiding (sequence)
import Data.List
import qualified Data.Map as M

import Ideas.Common.Library
import Ideas.Common.Strategy.Combinators hiding (not)
import Ideas.Text.XML.Interface(Element)

import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Parser
import Domain.Scenarios.Condition(evaluateMaybeCondition)
import Domain.Scenarios.Globals
import Domain.Scenarios.Id(toIdTypeSegment)

-- Make a rule with an identifier and a description, 
-- if the precondition is fulfilled given the state and apply the effects of the rule onto the state.
guardedRule :: IsId a => a -> String -> (ScenarioState -> Bool) -> (ScenarioState -> ScenarioState) -> Rule ScenarioState
guardedRule identifier description precondCheck applyEffects =
    describe description $ makeRule identifier (\state -> do guard $ precondCheck state; Just $ applyEffects state)
   
makeGuardedRule :: ID -> Statement -> Tree -> Rule ScenarioState
makeGuardedRule scenarioID statement tree = guardedRule
    (["scenarios", scenarioID, toIdTypeSegment (statType statement), statID statement]) -- create an identifier for the rule
    (either id (intercalate " // " . map snd) (statText statement))                                -- make a description for the rule
    (evaluateMaybeCondition (statPrecondition statement))                                          -- check if precondition is fulfilled
    (\state -> applyEffects state (statParamEffects statement) (statEmotionEffects statement))     -- apply the effects of a statement to the state
    --The initial state is not generated here. 
    --It is generated at exercises.hs then the frontend requests it with the "examples" method and sends it back with the first "allfirsts" request
            

-- Parses the dialogue from the script, sorts it with the level of interleaving and makes a strategy for each level          
makeStrategy :: Monad m => Script -> m (Strategy ScenarioState)
makeStrategy script = do
    let dialogue = scenarioDialogue (parseScenario script)
    let sortedDialogue = sortWith (\(level, _) -> level) dialogue
    let scenarioID = parseScenarioID script
    intLvlStrategies <- mapM (\intLvl -> makeIntLvlStrategy intLvl scenarioID) sortedDialogue
    return (sequence' intLvlStrategies)
      where 
        sequence' = Ideas.Common.Strategy.Combinators.sequence
        
-- For each tree (subject) in an interleave level make a strategy 
makeIntLvlStrategy :: Monad m => InterleaveLevel -> ID -> m (Strategy ScenarioState) 
makeIntLvlStrategy (_, trees) scenarioID = do
    treeStrategies <- mapM (\tree -> makeTreeStrategy tree scenarioID) trees
    return (interleave treeStrategies)
    
-- Recursively make a strategy for a tree by making a strategy for the starting statement,
-- then get the next statements and make a strategy for those statements and so on.
makeTreeStrategy :: Monad m => Tree -> ID -> m (Strategy ScenarioState)
makeTreeStrategy tree scenarioID = do
    let startID = treeStartID tree    
    strategy <- makeStatementStrategy tree scenarioID startID   
    return strategy

makeStatementStrategy :: Monad m => Tree -> ID -> ID -> m (Strategy ScenarioState)
makeStatementStrategy tree scenarioID statementID = do 
    -- Find the given statement in the tree with the id
    let statementErrorMsg = "Could not find statement: " ++ statementID ++ " in tree: " ++ treeID tree
    let maybeStatement = find (\stat -> statID stat == statementID) (treeStatements tree)
    let statement = errorOnFail statementErrorMsg maybeStatement
    
    let nextIDs = nextStatIDs statement    
    
    let rule = makeGuardedRule scenarioID statement tree
    
    case nextIDs of 
        [] -> return (toStrategy rule)
        _  -> do
            -- For each next statement make a strategy
            nextStrategyList <- mapM (makeStatementStrategy tree scenarioID) nextIDs
            
            -- Combine all possible next strategies with the choice operator
            let nextStrategy = alternatives nextStrategyList
            
            let strategy | jumpPoint statement                                 = rule <*> nextStrategy
                         | otherwise                                           = rule !~> nextStrategy -- the atomic prefix combinator, this combinator doesn't allow interleaving
            
            return strategy