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
type StrategyState = (Strategy EmotionalState, StrategyMap EmotionalState)

--framework code, try not to break your head.
guardedRule :: IsId b => b -> String -> (EmotionalState -> Bool) -> (EmotionalState -> EmotionalState) -> Rule EmotionalState
guardedRule identifier description check update =
    describe description $ makeRule identifier (\state -> do guard $ check state; Just $ update state)
    --make description and add it to the rule $ make the rule
   
makeGuardedRule :: ID -> Statement -> Tree -> Rule EmotionalState
makeGuardedRule scriptID statement tree = guardedRule
    (["scenarios", scriptID, toIdTypeSegment (statType statement), statID statement])
    (either id (intercalate " // " . map snd) (statDescription statement))
    (evaluateMaybeCondition (statPrecondition statement)) -- check if precondition is fulfilled
    (\state -> foldr applyEffect (fst state, treeID tree) (statEffects statement))
    --the initial state is not generated here. 
    --It is generated at exercises.hs then the frontend requests it with the "examples" method and sends it back with the first "allfirsts" request
            
            
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
    strategyTuple <- makeStatStrategy startID tree scriptID M.empty
    let treeStrategy = fst strategyTuple
    if (treeAtomic tree)
        then return (atomic treeStrategy)
        else return treeStrategy
    
makeStatementStrategy :: Monad m => ID -> Tree -> ID -> StrategyMap EmotionalState -> m StrategyState
makeStatementStrategy statementID tree scriptID strategyMap = do 
    let statement = errorOnFail $ find (\stat -> statID stat == statementID) (treeStatements tree)
    
    case M.lookup statementID strategyMap of
        
        Just statementStrategy -> return (statementStrategy, strategyMap)
        
        Nothing                -> do
            let rule = makeGuardedRule scriptID statement tree
            let nextIDs = nextStatIDs statement
            
            case nextIDs of 
                [] -> do 
                    let statementStrategy = atomic rule
                    return (statementStrategy, M.insert statementID statementStrategy strategyMap)
                    
                (nextID : restIDs)  -> do 
                    firstStrategyTuple <- makeStatementStrategy nextID tree scriptID strategyMap
                    (nextsStrategy, nextsStrategyMap) <- foldM makeNextStatementStrategy firstStrategyTuple restIDs 
                    let strategy = rule <*> nextsStrategy
                    return (strategy, M.insert statementID strategy nextsStrategyMap)
                
                      where 
                        makeNextStatementStrategy :: Monad m => StrategyState -> ID -> m StrategyState
                        makeNextStatementStrategy (stratSoFar, mapSoFar) nextID = do
                        (nextStrategy, nextMap) <- makeStatementStrategy nextID tree scriptID mapSoFar
                        return (stratSoFar <|> nextStrategy, nextMap)
                        
                        

makeStatStrategy :: Monad m => ID -> Tree -> ID -> StrategyMap EmotionalState -> m StrategyState  
makeStatStrategy statementID tree scriptID strategyMap = do
    (strategyState, statement) <- makeStrategyUntilJump 
    
    -- if there is a jump then make the strategy for the jump, if there is none, just return the normal strategy
    if (jumpPoint statement)
        then do 
            (jumpStrategyState, jumpMap) <- makeJumpStrategy statement tree scriptID strategyState
            return jumpStrategyState
        else do
            return strategyState
    
    
makeJumpStrategy :: Monad m => Statement -> Tree -> ID -> StrategyState -> m StrategyState 
makeJumpStrategy jumpStatement tree scriptID (strategy, strategyMap) = do
    let jumpRule = makeGuardedRule scriptID jumpStatement tree
    jumpStrategy <- (atomic strategy) <*> jumpRule
    --TODO: nextStrategyState 
    return (strategy, M.insert (statID jumpStatement) strategy strategyMap)

                        
makeStrategyUntilJump :: Monad m => ID -> Tree -> ID -> StrategyMap EmotionalState -> m (StrategyState, Statement)                      
makeStrategyUntilJump statementID tree scriptID strategyMap = do
    let statement = errorOnFail $ find (\stat -> statID stat == statementID) (treeStatements tree)
    
    case M.lookup statementID strategyMap of
        
        Just statementStrategy -> return (statementStrategy, strategyMap)
        
        Nothing                -> do
            let rule = makeGuardedRule scriptID statement tree
            let nextIDs = nextStatIDs statement
            return (makeNextIDsStrategy nextIDs statementID rule strategyMap, statement)
                    
                
makeNextIDsStrategy :: Monad m => [ID] -> ID -> Rule EmotionalState -> m StrategyState
makeNextIDsStrategy []                 statementID rule strategyMap = do 
    let statementStrategy = atomic rule
    return ((statementStrategy, M.insert statementID statementStrategy strategyMap), statement)
makeNextIDsStrategy (nextID : restIDs) statementID rule strategyMap = do
    firstStrategyTuple <- makeStatStrategy nextID tree scriptID strategyMap
    (nextsStrategy, nextsStrategyMap) <- foldM makeNextStatementStrategy firstStrategyTuple restIDs 
    let strategy = rule <*> nextsStrategy
    return (strategy, M.insert statementID strategy nextsStrategyMap)          
  where 
    makeNextStatementStrategy :: Monad m => StrategyState -> ID -> m StrategyState
    makeNextStatementStrategy (stratSoFar, mapSoFar) nextID = do
    (nextStrategy, nextMap) <- makeStatStrategy nextID tree scriptID mapSoFar
    return (stratSoFar <|> nextStrategy, nextMap)








