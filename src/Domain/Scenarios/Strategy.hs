module Domain.Scenarios.Strategy where

import GHC.Exts(sortWith)
import Prelude hiding (sequence)

import Data.Maybe
import Data.List hiding (inits)
import qualified Data.Map as M

import Control.Monad hiding (sequence)

import Ideas.Common.Library
import Ideas.Common.Strategy.Combinators hiding (not)
import Ideas.Text.XML.Interface(Element)

import Domain.Scenarios.ScenarioState
import Domain.Scenarios.Parser
import Domain.Scenarios.Condition(evaluateMaybeCondition)
import Domain.Scenarios.Globals
import Domain.Scenarios.Id(toIdTypeSegment)

type StrategyMap = M.Map ID (Strategy ScenarioState)

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
    (\state -> foldr applyEffect state (statEffects statement))                 -- apply the effects of a statement to the state
    --The initial state is not generated here. 
    --It is generated at exercises.hs then the frontend requests it with the "examples" method and sends it back with the first "allfirsts" request
            

-- Parses the dialogue from the script, sorts it with the level of interleaving and makes a strategy for each level          
makeStrategy :: Monad m => Script -> m (Strategy ScenarioState)
makeStrategy script = do
    let dialogue = scenarioDialogue (parseScenario script)
    let sortedDialogue = sortWith (\(level, _) -> level) dialogue
    let scenarioID = parseScenarioID script
    intLvlStrategies <- mapM (\intLvl -> makeIntLvlStrategy intLvl scenarioID) sortedDialogue
    return (sequence intLvlStrategies)
        
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
    (strategy, _) <- makeStatementStrategy M.empty tree scenarioID startID
    
    let treeStrategy | treeOptional tree && treeAtomic tree = option (atomic strategy)
                     | treeOptional tree                    = option strategy
                     | treeAtomic tree                      = atomic strategy
                     | otherwise                            = strategy
    
    return treeStrategy

makeStatementStrategy :: Monad m => StrategyMap -> Tree -> ID -> ID -> m (Strategy ScenarioState, StrategyMap)
makeStatementStrategy strategyMap tree scenarioID statementID = do 
    case M.lookup statementID strategyMap of
        Just strategy -> return (strategy, strategyMap)
        Nothing       -> do
            -- Find the given statement in the tree with the id
            let statementErrorMsg = "Could not find statement: " ++ statementID ++ " in tree: " ++ treeID tree
            let maybeStatement = find (\stat -> statID stat == statementID) (treeStatements tree)
            let statement = errorOnFail statementErrorMsg maybeStatement
        
            let nextIDs = nextStatIDs statement        
            let rule = makeGuardedRule scenarioID statement tree
    
            case nextIDs of 
                [] -> do 
                    let statementStrategy = toStrategy rule
                    return (statementStrategy, M.insert statementID statementStrategy strategyMap)
                    
                (firstNextID : tailNextIDs)  -> do
                    firstStrategyTuple <- makeStatementStrategy strategyMap tree scenarioID firstNextID
                
                    -- For each next statement make a strategy and fold with choice
                    (nextStrategy, nextStrategyMap) <- foldM foldAlternatives firstStrategyTuple tailNextIDs
                    
                    let statementStrategy | jumpPoint statement && statInits statement   = rule <*> inits nextStrategy
                                 | jumpPoint statement                          = rule <*> nextStrategy
                                 | statInits statement && not (treeAtomic tree) = rule !~> inits nextStrategy
                                 | treeAtomic tree                              = rule <*> nextStrategy 
                                 | otherwise                                    = rule !~> nextStrategy
                                 
                    return (statementStrategy, M.insert statementID statementStrategy nextStrategyMap)
  where 
    foldAlternatives :: Monad m => (Strategy ScenarioState, StrategyMap) -> ID -> m (Strategy ScenarioState, StrategyMap)
    foldAlternatives (strategySoFar, strategyMap) nextID = do
        (nextStrategy, newStrategyMap) <- makeStatementStrategy strategyMap tree scenarioID nextID 
        return (strategySoFar <|> nextStrategy, newStrategyMap)