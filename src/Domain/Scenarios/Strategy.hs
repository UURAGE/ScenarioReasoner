module Domain.Scenarios.Strategy where

import GHC.Exts(sortWith)

import Control.Monad hiding (sequence)
import Data.List
import qualified Data.Map as M

import Ideas.Common.Library
import Ideas.Common.Strategy.Combinators

import Domain.Scenarios.Types
import Domain.Scenarios.Parser
import Ideas.Text.XML.Interface(Element)

type StrategyMap a = M.Map ID (Strategy a)

--framework code, try not to break your head.
guardedRule :: IsId b => b -> String -> (a -> Bool) -> (a -> a) -> Rule a
guardedRule identifier description check update =
    describe description $ makeRule identifier (\state -> do guard $ check state; Just $ update state)
    --make description and add it to the rule $ make the rule
            
makeStrategy :: Monad m => Script -> m (Strategy EmotionalState)
makeStrategy script = do
    dialogue <- parseDialogue script
    let sortedDialogue = sortWith (\(level, _) -> level) dialogue
    scriptID <- getScriptId script
    intLvlStrategies <- mapM (\intLvl -> makeIntLvlStrategy intLvl scriptID) sortedDialogue
    return (sequence' intLvlStrategies)
      where 
        sequence' = Ideas.Common.Strategy.Combinators.sequence
        
makeIntLvlStrategy :: Monad m => InterleaveLevel -> ID -> m (Strategy EmotionalState) 
makeIntLvlStrategy (_, trees) scriptID = do
    treeStrategies <- mapM (\tree -> makeTreeStrategy tree scriptID) trees
    return (interleave treeStrategies)
    
makeTreeStrategy :: Monad m => Tree -> ID -> m (Strategy EmotionalState)
makeTreeStrategy tree scriptID = do
    let startID = treeStartID tree
    strategyTuple <- makeStatementStrategy startID tree scriptID M.empty
    let treeStrategy = fst strategyTuple
    if (treeAtomic tree)
        then return (atomic treeStrategy)
        else return treeStrategy
    
makeStatementStrategy :: Monad m => ID -> Tree -> ID -> StrategyMap EmotionalState -> m (Strategy EmotionalState, StrategyMap EmotionalState)
makeStatementStrategy statementID tree scriptID strategyMap = do 
    let statement = errorOnFail $ find (\stat -> statID stat == statementID) (treeStatements tree)
    
    case M.lookup statementID strategyMap of
        
        Just statementStrategy -> return (statementStrategy, strategyMap)
        
        Nothing                -> do
            let rule = guardedRule
                    (["scenarios", scriptID, toIdTypeSegment (statType statement), statementID])
                    (either id (intercalate " // " . map snd) (statDescription statement))
                    (evaluateMaybeCondition (statPrecondition statement)) -- check if precondition is fulfilled
                    (\state -> foldr applyEffect (fst state, treeID tree) (statEffects statement))
                    --the initial state is not generated here. 
                    --It is generated at exercises.hs then the frontend requests it with the "examples" method and sends it back with the first "allfirsts" request
            let nextIDs = nextStatIDs statement
            
            case nextIDs of 
                [] -> do 
                    let statementStrategy = atomic rule
                    return (statementStrategy, M.insert statementID statementStrategy strategyMap)
                    
                (nextID : restIDs)  -> do 
                    firstStrategyTuple <- makeStatementStrategy nextID tree scriptID strategyMap
                    (nextsStrategy, nextsStrategyMap) <- foldM (makeNextStatementStrategy tree scriptID) firstStrategyTuple restIDs 
                    let strategy = addRuleToStrategy rule nextsStrategy (statJump statement)
                    return (strategy, M.insert statementID strategy nextsStrategyMap)
                
                      where 
                        makeNextStatementStrategy :: Monad m => Tree -> ID -> (Strategy EmotionalState, StrategyMap EmotionalState) -> ID -> m (Strategy EmotionalState, StrategyMap EmotionalState)
                        makeNextStatementStrategy tree scriptID (stratSoFar, mapSoFar) nextID = do
                        (nextStrategy, nextMap) <- makeStatementStrategy nextID tree scriptID mapSoFar
                        return (stratSoFar <|> nextStrategy, nextMap)
    

addRuleToStrategy :: Rule a -> Strategy a -> Bool -> Strategy a
addRuleToStrategy rule strategy True  = rule <*> strategy
addRuleToStrategy rule strategy False = (atomic rule) <*> strategy














