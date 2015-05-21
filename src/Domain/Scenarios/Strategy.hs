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
import Domain.Scenarios.Id

type StrategyMap = M.Map ID (Strategy ScenarioState)

-- Make a rule with an identifier and a description, 
-- if the precondition is fulfilled given the state and apply the effects of the rule onto the state.
guardedRule :: IsId a => a -> String -> (ScenarioState -> Bool) -> (ScenarioState -> ScenarioState) -> Rule ScenarioState
guardedRule identifier description precondCheck applyEffects =
    describe description $ makeRule identifier (\state -> do guard $ precondCheck state; Just $ applyEffects state)
   
makeGuardedRule :: ID -> Statement -> Tree -> Rule ScenarioState
makeGuardedRule scenarioID statement tree = guardedRule
    ["scenarios", scenarioID, statType (statInfo statement), statID statement]                                          -- create an identifier for the rule
    (either id (intercalate " // " . map snd) (statText (statInfo statement)))                                          -- make a description for the rule
    (evaluateMaybeCondition (statPrecondition statement))                                                               -- check if precondition is fulfilled
    (\state -> applyEffects state (statParamEffects statement) (statEmotionEffects statement) (statInfo statement))     -- apply the effects of a statement to the state
    --The initial state is not generated here. 
    --It is generated at exercises.hs then the frontend requests it with the "examples" method and sends it back with the first "allfirsts" request
            

-- Parses the dialogue from the script, sorts it with the level of interleaving and makes a strategy for each level          
makeStrategy :: Script -> Strategy ScenarioState
makeStrategy script = sequence (map (makeIntLvlStrategy scenarioID) sortedDialogue)
  where
    dialogue = scenarioDialogue (parseScenario script)
    sortedDialogue = sortWith fst dialogue
    scenarioID = parseScenarioID script
        
-- For each tree (subject) in an interleave level make a strategy 
makeIntLvlStrategy :: ID -> InterleaveLevel -> Strategy ScenarioState
makeIntLvlStrategy scenarioID (_, trees) = interleave (map (makeTreeStrategy scenarioID) trees)
    
-- Recursively make a strategy for a tree by making a strategy for the starting statement,
-- then get the next statements and make a strategy for those statements and so on.
makeTreeStrategy :: ID -> Tree-> Strategy ScenarioState
makeTreeStrategy scenarioID tree  
    | treeOptional tree && treeAtomic tree = option (atomic treeStrategy)
    | treeOptional tree                    = option treeStrategy
    | treeAtomic tree                      = atomic treeStrategy
    | otherwise                            = treeStrategy
  where 
    (treeStrategy, _) = makeAlternativesStrategy M.empty tree scenarioID (treeStartIDs tree)

makeStatementStrategy :: StrategyMap -> Tree -> ID -> ID -> (Strategy ScenarioState, StrategyMap)
makeStatementStrategy strategyMap tree scenarioID statementID = 
    case M.lookup statementID strategyMap of
        Just strategy -> (strategy, strategyMap)
        Nothing       -> 
            case nextStatIDs statement of 
                []       -> (toStrategy rule, M.insert statementID (toStrategy rule) strategyMap)
                nextIDs  -> (statementStrategy, M.insert statementID statementStrategy nextStrategyMap)
                      where 
                        (nextStrategy, nextStrategyMap) = makeAlternativesStrategy strategyMap tree scenarioID nextIDs
                        statementStrategy = sequenceRule statement tree rule nextStrategy
          where 
            -- Find the given statement in the tree with the id
            statementErrorMsg = "Could not find statement: " ++ statementID ++ " in tree: " ++ treeID tree
            maybeStatement = find (\stat -> statID stat == statementID) (treeStatements tree)
            statement = errorOnFail statementErrorMsg maybeStatement
                
            rule = makeGuardedRule scenarioID statement tree
             
makeAlternativesStrategy :: StrategyMap -> Tree -> ID -> [ID] -> (Strategy ScenarioState, StrategyMap)   
makeAlternativesStrategy strategyMap tree scenarioID (statID : []) = 
    makeStatementStrategy strategyMap tree scenarioID statID
makeAlternativesStrategy strategyMap tree scenarioID (firstStatID : statIDs) = 
    foldl (foldAlternatives tree scenarioID) firstStrategy statIDs
  where firstStrategy = makeStatementStrategy strategyMap tree scenarioID firstStatID
            
sequenceRule :: Statement -> Tree -> Rule ScenarioState -> Strategy ScenarioState -> Strategy ScenarioState
sequenceRule statement tree rule nextStrategy 
    | jumpPoint statement && statInits statement   = rule <*> inits nextStrategy
    | jumpPoint statement                          = rule <*> nextStrategy
    | statInits statement && treeAtomic tree       = rule <*> inits nextStrategy
    | statInits statement && not (treeAtomic tree) = rule !~> inits nextStrategy
    | treeAtomic tree                              = rule <*> nextStrategy 
    | otherwise                                    = rule !~> nextStrategy
    
foldAlternatives :: Tree -> ID -> (Strategy ScenarioState, StrategyMap) -> ID -> (Strategy ScenarioState, StrategyMap)
foldAlternatives tree scenarioID (strategySoFar, strategyMap) statementID = (strategySoFar <|> nextStrategy, newStrategyMap)
  where  (nextStrategy, newStrategyMap) = makeStatementStrategy strategyMap tree scenarioID statementID 