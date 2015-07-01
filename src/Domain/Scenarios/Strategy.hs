------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

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
import Domain.Scenarios.Scenario
import Domain.Scenarios.Condition(evaluateMaybeCondition)
import Domain.Scenarios.Globals

type StrategyMap = M.Map ID (Strategy ScenarioState)

-- Sorts the dialogue with the level of interleaving trees / subjects and makes a strategy for each level          
makeStrategy :: ID -> Dialogue -> Strategy ScenarioState
makeStrategy scenarioID dialogue = sequence (map (makeInterleaveStrategy scenarioID) sortedDialogue)
  where
    sortedDialogue = sortWith fst dialogue
        
-- For each tree (subject) in an interleave level make a strategy 
makeInterleaveStrategy :: ID -> InterleaveLevel -> Strategy ScenarioState
makeInterleaveStrategy scenarioID (_, trees) = interleave (map (makeTreeStrategy scenarioID) trees)
    

makeTreeStrategy :: ID -> Tree-> Strategy ScenarioState
makeTreeStrategy scenarioID tree  
    | treeOptional tree && treeAtomic tree = option (atomic treeStrategy)
    | treeOptional tree                    = option treeStrategy
    | treeAtomic tree                      = atomic treeStrategy
    | otherwise                            = treeStrategy
  where 
    (treeStrategy, _) = makeAlternativesStrategy M.empty tree scenarioID (treeStartIDs tree)

-- Recusively make a strategy for a tree of statements by making a strategy for the starting statement,
-- and then sequence it with the next strategy for the next statements.
makeStatementStrategy :: StrategyMap -> Tree -> ID -> ID -> (Strategy ScenarioState, StrategyMap)
makeStatementStrategy strategyMap tree scenarioID statementID = 
    -- If the strategy for the statement has already been computed, use that one otherwise compute it.
    case M.lookup statementID strategyMap of
        Just strategy -> (strategy, strategyMap)
        Nothing       -> 
            case nextStatIDs statement of 
                []       -> (toStrategy rule, M.insert statementID (toStrategy rule) strategyMap)
                nextIDs  -> (statementStrategy, M.insert statementID statementStrategy nextStrategyMap)
                      where 
                        -- Make a strategy of alternative strategies for the strategies following from the rule
                        (nextStrategy, nextStrategyMap) = makeAlternativesStrategy strategyMap tree scenarioID nextIDs
                        
                        -- Sequence the rule to the strategy following from the rule
                        statementStrategy = sequenceRule statement tree rule nextStrategy
          where 
            -- Find the given statement in the tree with the statementID
            statementErrorMsg = "Could not find statement: " ++ statementID ++ " in tree: " ++ treeID tree
            maybeStatement = find (\stat -> statID stat == statementID) (treeStatements tree)
            statement = errorOnFail statementErrorMsg maybeStatement
                
            -- Make the rule for the current statement
            rule = makeGuardedRule scenarioID statement tree
    
-- Folds over all the next statements, makes strategies for them
-- and then combines them with the choice operator.
makeAlternativesStrategy :: StrategyMap -> Tree -> ID -> [ID] -> (Strategy ScenarioState, StrategyMap)   
makeAlternativesStrategy strategyMap tree scenarioID (statID : []) = 
    makeStatementStrategy strategyMap tree scenarioID statID
makeAlternativesStrategy strategyMap tree scenarioID (firstStatID : statIDs) = 
    foldl (foldAlternatives tree scenarioID) firstStrategy statIDs
  where 
    firstStrategy = makeStatementStrategy strategyMap tree scenarioID firstStatID
    
    foldAlternatives :: Tree -> ID -> (Strategy ScenarioState, StrategyMap) -> ID -> (Strategy ScenarioState, StrategyMap)
    foldAlternatives tree scenarioID (stratSoFar, stratMap) statID = (stratSoFar <|> nextStrategy, newStrategyMap)
      where  (nextStrategy, newStrategyMap) = makeStatementStrategy stratMap tree scenarioID statID 
      
-- Make a rule using all the specific properties for a scenario 
makeGuardedRule :: ID -> Statement -> Tree -> Rule ScenarioState
makeGuardedRule scenarioID statement tree = guardedRule
    ["scenarios", scenarioID, statType (statInfo statement), statID statement]          -- create an identifier for the rule
    (either id (intercalate " // " . map snd) (statText (statInfo statement)))          -- make a description for the rule
    (evaluateMaybeCondition (statPrecondition statement))                               -- check if precondition is fulfilled
    (\state -> applyEffects state parameterEffects emotionEffects (statInfo statement)) -- apply the effects of a statement to the state
  where
    -- Make a rule with an identifier and a description, 
    -- if the precondition is fulfilled given the state and apply the effects of the rule onto the state.
    guardedRule :: IsId a => a -> String -> (ScenarioState -> Bool) -> (ScenarioState -> ScenarioState) -> Rule ScenarioState
    guardedRule identifier description precondCheck applyEffects =
        describe description $ makeRule identifier (\state -> do guard $ precondCheck state; Just $ applyEffects state)
    
    parameterEffects = statParamEffects statement
    emotionEffects = statEmotionEffects statement
 
-- Sequence a rule with the atomic combinator (!~>) if the tree is not atomic, but the statement itself is, 
-- so it can not be interleaved and apply the inits operator if the tree can succeed here,
-- so the strategy does not have to be finished.
sequenceRule :: Statement -> Tree -> Rule ScenarioState -> Strategy ScenarioState -> Strategy ScenarioState
sequenceRule statement tree rule nextStrategy 
    | jumpPoint statement && statInits statement   = rule <*> inits nextStrategy
    | jumpPoint statement                          = rule <*> nextStrategy
    | statInits statement && treeAtomic tree       = rule <*> inits nextStrategy
    | statInits statement && not (treeAtomic tree) = rule !~> inits nextStrategy
    | treeAtomic tree                              = rule <*> nextStrategy 
    | otherwise                                    = rule !~> nextStrategy    
