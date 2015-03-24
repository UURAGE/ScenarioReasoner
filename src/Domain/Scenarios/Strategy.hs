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
    treeElemTuples <- getTrees script
    scriptId <- getScriptId script
    strategies <- mapM (mapM (\treeElemTuple -> makeTreeStrategy treeElemTuple scriptId (treeStartID $ fst treeElemTuple))) treeElemTuples -- one strategy per tree
    return $ sequence' (map interleave strategies) -- transform nested list into sequence of interleaves
    where
        sequence' = Ideas.Common.Strategy.Combinators.sequence
        
        
makeNewStrategy :: Monad m => Script -> m (Strategy EmotionalState)
makeNewStrategy script = undefined
    

makeTreeStrategy :: Monad m => (Tree, Element) -> String -> String -> m (Strategy EmotionalState)
makeTreeStrategy tuple@(tree,_) scriptId statementId = do
    strategyTuple <- makeSubStrategy tuple scriptId M.empty statementId
    let strategy = fst strategyTuple
    if(treeAtomic tree)
        then return (atomic strategy)
        else return strategy


--sub strats make a strat for one tree, so it should not be too hard to expand it once we know the starting statements of each tree.
makeSubStrategy :: Monad m => (Tree, Element) -> String -> StrategyMap EmotionalState -> String -> m (Strategy EmotionalState, StrategyMap EmotionalState)
makeSubStrategy (tree,  treeElem) scriptId strategyMap statementId = do

    let statement = head $ findStatementAt treeElem statementId
    statementType         <- getType statement
    statementDescription  <- getText statement
    statementPrecondition <- getMaybePrecondition statement
    statementEffects      <- getEffects statement

    case M.lookup statementId strategyMap of --check if statement is already in the strategy

        Just statementStrategy -> return (statementStrategy, strategyMap) --if it is already in the strategy do nothing and just return the strategy

        Nothing -> do
            let rule = guardedRule
                    (["scenarios", scriptId, toIdTypeSegment statementType, statementId])
                    (either id (intercalate " // " . map snd) statementDescription)

                    (evaluateMaybeCondition statementPrecondition) -- check if precondition is fulfilled
                    (\state -> foldr applyEffect (fst state, treeID tree) statementEffects) -- apply effects of node
                    --the initial state is not generated here. It is generated at exercises.hs then the frontend requests it with the "examples" method and sends it back with the first "allfirsts" request
            nextIds <- getNexts statement

            case nextIds of

                []                        -> do -- end of conversation path, return substrategy so far
                    let statementStrategy = atomic rule
                    return (statementStrategy, M.insert statementId statementStrategy strategyMap)

                firstNextId : restNextIds -> do -- process all possible choices
                    firstStrategyTuple <- makeSubStrategy (tree, treeElem) scriptId strategyMap firstNextId
                    (foldedStrategy, nextsStrategyMap) <- foldM folder firstStrategyTuple restNextIds
                    let statementStrategy = rule <*> foldedStrategy
                    return (statementStrategy, M.insert statementId statementStrategy nextsStrategyMap)

    where folder (stratSoFar, rulesSoFar) nextId = do
            (newStrat, newRules) <- makeSubStrategy (tree, treeElem) scriptId rulesSoFar nextId
            return (stratSoFar <|> newStrat, newRules)
            
makeDialogueStrategy :: Monad m => Script -> m (Strategy EmotionalState)
makeDialogueStrategy script = do
    dialogue <- parseDialogue script
    let sortedDialogue = sortWith (\(level, _) -> level) dialogue
    scriptID <- getScriptId script
    intLvlStrategies <- mapM (\intLvl -> makeIntLvlStrategy intLvl scriptID) sortedDialogue
    return (sequence' intLvlStrategies)
      where 
        sequence' = Ideas.Common.Strategy.Combinators.sequence
        
makeIntLvlStrategy :: Monad m => InterleaveLevel -> ID -> m (Strategy EmotionalState) 
makeIntLvlStrategy (_, trees) scriptID = do
    treeStrategies <- mapM (\tree -> makeTreStrategy tree scriptID) trees
    return (interleave treeStrategies)
    
makeTreStrategy :: Monad m => Tree -> ID -> m (Strategy EmotionalState)
makeTreStrategy tree scriptID = do
    let startID = treeStartID tree
    treeStrategy <- makeStatementStrategy startID tree scriptID
    if (treeAtomic tree)
        then return (atomic treeStrategy)
        else return treeStrategy
    
makeStatementStrategy :: Monad m => ID -> Tree -> ID -> StrategyMap EmotionalState -> m (Strategy EmotionalState)
makeStatementStrategy statementID tree scriptID strategyMap = do 
    let statement = find (\stat -> statID stat == statementID) (treeStatements tree)
    
    case M.lookup statementID strategyMap of
        
        Just statementStrategy -> return (statementStrategy, strategyMap)
        
        Nothing                -> do
            let rule = guardedRule
                    (["scenarios", scriptID, toIdTypeSegment (statType statement), statementID]
                    (either id (intercalate " // " . map snd) (statDescription statement))
                    (evaluateMaybeCondition (statPrecondition statement)) -- check if precondition is fulfilled
                    (\state -> foldr applyEffect (fst state, treeID tree) (statEffects statement)))
                    --the initial state is not generated here. 
                    --It is generated at exercises.hs then the frontend requests it with the "examples" method and sends it back with the first "allfirsts" request
            nextIDs <- nextStatIDs statement
            
            case nextIDs of 
                [] -> do 
                    let statementStrategy = atomic rule
                    return (statementStrategy, M.insert statementId statementStrategy strategyMap)
                    
                _  -> do 
                    statementStrategy <- makeNextStatementStrategy statementID rule nextIDs tree scriptID strategyMap 
                    return statementStrategy