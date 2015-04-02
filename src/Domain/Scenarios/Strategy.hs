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

type StrategyMap a = M.Map ID (Strategy a)
type StrategyState = (Strategy EmotionalState, StrategyMap EmotionalState)

--framework code, try not to break your head.
guardedRule :: IsId b => b -> String -> (EmotionalState -> Bool) -> (EmotionalState -> EmotionalState) -> Rule EmotionalState
guardedRule identifier description check update =
    describe description $ makeRule identifier (\state -> do guard $ check state; Just $ update state)
    --make description and add it to the rule $ make the rule
   
makeGuardedRule :: ID -> Statement -> Tree -> String -> Rule EmotionalState
makeGuardedRule scriptID statement tree interleaved = guardedRule
    (["scenarios", scriptID, toIdTypeSegment (statType statement), statID statement, interleaved])
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
    
    strategy <- makeRealStatStrategy tree scriptID startID
    
    return strategy
    
{-
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
-}

{-
-- de versie als je bovenaan een sequence begint (na een boombegin, of als je net uit een jumppoint komt)
makeStatStrategy :: Monad m => Maybe (Strategy EmotionalState) -> ID -> Tree -> ID -> m (Strategy EmotionalState)
makeStatStrategy Nothing statementID tree scriptID = do 
    -- Vind het gekozen statement door in de boom te zoeken op ID
    let statement = errorOnFail $ find (\stat -> statID stat == statementID) (treeStatements tree)
    -- maak een regel aan 
    let rule = makeGuardedRule scriptID statement tree
    -- haal de volgende mogelijke statmentkeuzes (ID) uit de boom
    let nextIDs = nextStatIDs statement
    
    case nextIDs of 
        -- geef een atomic regel als je aan het eind van een tak zit
        [] -> return (atomic rule)
        -- anders:
        _  -> do
            -- vind de volgende mogelijke statements
            let nextStats = filter (\s -> elem (statID s) nextIDs) (treeStatements tree) 
            -- handle elk statement recursief af
            nextStrategies <- mapM (handleJumpPoint statement rule) nextStats
            
            return (alternatives nextStrategies)
    where
        -- functie die van een statement en regel een (deel)strategie maken, gebaseerd op of het een jumppoint is of niet
        -- als het volgende statement een jumppoint is dan moeten we de rule atomair maken
        -- als dit statement een jumppoint is dan maakt het niet uit of het volgende een jumpPoint is
        -- anders geef de strategy tot nu toe door
        handleJumpPoint :: Monad m => Statement -> Rule EmotionalState -> Statement -> m (Strategy EmotionalState)
        handleJumpPoint statement rule nextStatement
            |  jumpPoint nextStatement && not (jumpPoint statement) = do
                nextStrategy <- makeStatStrategy Nothing (statID nextStatement) tree scriptID
                return ( (atomic (atomic rule)) <*> nextStrategy)           
            |  jumpPoint statement = do
                nextStrategy <- makeStatStrategy Nothing (statID nextStatement) tree scriptID
                return (rule <*> nextStrategy)
            |  otherwise            = do
                nextStrategy <- makeStatStrategy (Just (atomic rule)) (statID nextStatement) tree scriptID
                return nextStrategy
                
-- versie als je verder gaat uit een vorige boom
makeStatStrategy (Just strategySoFar) statementID tree scriptID = do                    
    -- Vind het gekozen statement door in de boom te zoeken op ID
    let statement = errorOnFail $ find (\stat -> statID stat == statementID) (treeStatements tree)
    -- maak een regel aan 
    let rule = makeGuardedRule scriptID statement tree
    -- haal de volgende mogelijke statementkeuzes (ID) uit de boom
    let nextIDs = nextStatIDs statement
    
    case nextIDs of 
        -- geef een atomic regel als je aan het eind van een tak zit
        [] -> return (atomic (strategySoFar <*> rule))
        
        _  -> do
            -- vind de volgende mogelijke statements
            let nextStats = filter (\s -> elem (statID s) nextIDs) (treeStatements tree) 
            -- handle elk statement recursief af
            nextStrategies <- mapM (handleJumpPoint statement rule) nextStats
            
            return (alternatives nextStrategies)
            
    where 
        handleJumpPoint :: Monad m => Statement -> Rule EmotionalState -> Statement -> m (Strategy EmotionalState)
        handleJumpPoint statement rule nextStatement
            |  not (jumpPoint statement) && jumpPoint nextStatement = do
                nextStrategy <- makeStatStrategy Nothing (statID nextStatement) tree scriptID
                return ((atomic (strategySoFar <*> rule)) <*> nextStrategy)
            |  jumpPoint statement = do
                nextStrategy <- makeStatStrategy Nothing (statID nextStatement) tree scriptID
                return ((atomic strategySoFar) <*> rule <*> nextStrategy)
            |  otherwise = do
                nextStrategy <- makeStatStrategy (Just (strategySoFar <*> rule)) (statID nextStatement) tree scriptID
                return nextStrategy
-}
                
{-               
makeNextIDsStrategy :: Monad m => [ID] -> Tree -> ID -> ID -> StrategyMap EmotionalState -> m StrategyState
makeNextIDsStrategy (nextID : restIDs) scriptID tree previousID strategyMap = do
    firstStrategyTuple <- makeStatStrategy nextID tree scriptID strategyMap
    (nextsStrategy, nextsStrategyMap) <- foldM makeNextStatementStrategy firstStrategyTuple restIDs 
    return (nextsStrategy, nextsStrategyMap)          
  where 
    makeNextStatementStrategy :: Monad m => StrategyState -> ID -> m StrategyState
    makeNextStatementStrategy (stratSoFar, mapSoFar) nextID = do
    (nextStrategy, nextMap) <- makeStatStrategy nextID tree scriptID mapSoFar
    return (stratSoFar <|> nextStrategy, nextMap)
-}  


makeRealStatStrategy :: Monad m => Tree -> ID -> ID -> m (Strategy EmotionalState)
makeRealStatStrategy tree scriptID statementID = do 
    -- Vind het gekozen statement door in de boom te zoeken op ID
    let statement = errorOnFail $ find (\stat -> statID stat == statementID) (treeStatements tree)
    -- maak een regel aan, als er geinterleaved moet worden, wordt "interleaved" meegegeven aan de rule id (HACK) 
    let rule | jumpPoint statement = makeGuardedRule scriptID statement tree "interleaved"
             | otherwise           = makeGuardedRule scriptID statement tree "" 
                        
    -- haal de volgende mogelijke statmentkeuzes (ID) uit de boom
    let nextIDs = nextStatIDs statement
    
    case nextIDs of 
        [] -> return (toStrategy rule)
        _  -> do
            -- handle elk statement recursief af
            nextStrategyList <- mapM (makeRealStatStrategy tree scriptID) nextIDs
            
            let nextStrategy = alternatives nextStrategyList
            
            let strategy = rule <*> nextStrategy
                         
            return strategy 
            
{-            
makeStatementStrategy :: Monad m => Maybe (Strategy EmotionalState) -> ID -> Tree -> ID -> m [Strategy EmotionalState]
makeStatementStrategy Nothing statementID tree scriptID = do 
    -- Vind het gekozen statement door in de boom te zoeken op ID
    let statement = errorOnFail $ find (\stat -> statID stat == statementID) (treeStatements tree)
    -- maak een regel aan 
    let rule = makeGuardedRule scriptID statement tree
    -- haal de volgende mogelijke statmentkeuzes (ID) uit de boom
    let nextIDs = nextStatIDs statement
    
    case nextIDs of 
        -- geef een atomic regel als je aan het eind van een tak zit
        [] -> return [atomic rule]
        -- anders:
        _  -> do
            -- vind de volgende mogelijke statements
            let nextStats = filter (\s -> elem (statID s) nextIDs) (treeStatements tree) 
            -- handle elk statement recursief af
            nextStrategies <- mapM (handleJumpPoint statement rule) nextStats
            
            return nextStrategies
    where
        -- functie die van een statement en regel een (deel)strategie maken, gebaseerd op of het een jumppoint is of niet
        -- als het volgende statement een jumppoint is dan moeten we de rule atomair maken
        -- als dit statement een jumppoint is dan maakt het niet uit of het volgende een jumpPoint is
        -- anders geef de strategy tot nu toe door
        handleJumpPoint :: Monad m => Statement -> Rule EmotionalState -> Statement -> m (Strategy EmotionalState)
        handleJumpPoint statement rule nextStatement
            |  jumpPoint nextStatement && not (jumpPoint statement) = do
                nextStrategies <- makeStatementStrategy Nothing (statID nextStatement) tree scriptID
                return ( (atomic (atomic rule)) <*> (alternatives nextStrategies))           
            |  jumpPoint statement = do
                nextStrategies <- makeStatementStrategy Nothing (statID nextStatement) tree scriptID
                return (rule <*> (alternatives nextStrategies))
            |  otherwise            = do
                nextStrategies <- makeStatementStrategy (Just (atomic rule)) (statID nextStatement) tree scriptID
                return (alternatives nextStrategies)
                
-- versie als je verder gaat uit een vorige boom
makeStatementStrategy (Just strategySoFar) statementID tree scriptID = do                    
    -- Vind het gekozen statement door in de boom te zoeken op ID
    let statement = errorOnFail $ find (\stat -> statID stat == statementID) (treeStatements tree)
    -- maak een regel aan 
    let rule = makeGuardedRule scriptID statement tree
    -- haal de volgende mogelijke statementkeuzes (ID) uit de boom
    let nextIDs = nextStatIDs statement
    
    case nextIDs of 
        -- geef een atomic regel als je aan het eind van een tak zit
        [] -> return [atomic (strategySoFar <*> rule)]
        
        _  -> do
            -- vind de volgende mogelijke statements
            let nextStats = filter (\s -> elem (statID s) nextIDs) (treeStatements tree) 
            -- handle elk statement recursief af
            nextStrategies <- mapM (handleJumpPoint statement rule) nextStats
            
            return nextStrategies
            
    where 
        handleJumpPoint :: Monad m => Statement -> Rule EmotionalState -> Statement -> m (Strategy EmotionalState)
        handleJumpPoint statement rule nextStatement
            |  not (jumpPoint statement) && jumpPoint nextStatement = do
                nextStrategies <- makeStatementStrategy Nothing (statID nextStatement) tree scriptID
                return ((atomic (strategySoFar <*> rule)) <*> (alternatives nextStrategies))
            |  jumpPoint statement = do
                nextStrategies <- makeStatementStrategy Nothing (statID nextStatement) tree scriptID
                return ((atomic strategySoFar) <*> rule <*> (alternatives nextStrategies))
            |  otherwise = do
                nextStrategies <- makeStatementStrategy (Just (strategySoFar <*> rule)) (statID nextStatement) tree scriptID
                return (alternatives nextStrategies)

-}




