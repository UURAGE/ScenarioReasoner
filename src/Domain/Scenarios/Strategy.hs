module Domain.Scenarios.Strategy where

import Control.Monad hiding (sequence)
import Data.List
import qualified Data.Map as M

import Ideas.Common.Library
import Ideas.Common.Strategy.Combinators

import Domain.Scenarios.Types
import Domain.Scenarios.Parser

type StrategyMap a = M.Map String (Strategy a)

-- add some comment to test the push of git!!

--framework code, try not to break your head.
guardedRule :: IsId b => b -> String -> (a -> Bool) -> (a -> a) -> Rule a
guardedRule identifier description check update =
    describe description $ makeRule identifier (\x -> do guard $ check x; Just $ update x)
    --make description and add it to the rule $ make the rule

makeStrategy :: Monad m => Script -> m (Strategy State)
makeStrategy script = do
    startId <- getScriptStartId script
    treeElemTuples <- getTrees script
    scriptId <- getScriptId script
    tupleStrategies <- mapM (mapM (\tuple -> makeTreeStrategy tuple scriptId (startID $ fst tuple))) treeElemTuples -- one strategy per tree
    let strategies = map (map fst) tupleStrategies -- only first of tuples
    return $ sequence' (map interleave strategies) -- transform nested list into sequence of interleaves
    where
        sequence' = Ideas.Common.Strategy.Combinators.sequence

makeTreeStrategy :: Monad m => (Tree, TreeElement) -> String -> String -> m (Strategy State, StrategyMap State)
makeTreeStrategy tuple@(tree,  t@(TreeElement el)) scriptId statementId = do

    (strategy, strategyMap) <- makeSubStrategy tuple scriptId M.empty statementId
    if(treeAtomic tree == "true")
        then return (atomic strategy, strategyMap)
        else return (strategy, strategyMap)


--sub strats make a strat for one tree, so it should not be too hard to expand it once we know the starting statements of each tree.
makeSubStrategy :: Monad m => (Tree, TreeElement) -> String -> StrategyMap State -> String -> m (Strategy State, StrategyMap State)
makeSubStrategy (tree,  t@(TreeElement el)) scriptId strategyMap statementId = do

    let statement = head $ findStatementAt el statementId
    statementType         <- getType statement
    statementDescription  <- getText statement
    statementPrecondition <- getMaybePrecondition statement
    statementEffects      <- getEffects statement
	statEnd               <- getEnd  statement == "true"
	statJump              <- getJump statement == "true"

    case M.lookup statementId strategyMap of --check if statement is already in the strategy

        Just statementStrategy -> return (statementStrategy, strategyMap) --if it is already in the strategy do nothing and just return the strategy

        Nothing -> do
            let rule = guardedRule
                    (["scenarios", scriptId, toIdTypeSegment statementType, statementId])
                    (either id (intercalate " // " . map snd) statementDescription)

                    (calculateMaybeCondition statementPrecondition) -- check if precondition is fulfilled
                    (\state -> foldr applyEffect (fst state, treeID tree) statementEffects) -- apply effects of node
                    --the initial state is not generated here. It is generated at exercises.hs then the frontend requests it with the "examples" method and sends it back with the first "allfirsts" request
            nextIds <- getNexts statement

            case nextIds of

                []                        -> do -- end of conversation path, return substrategy so far
                    let statementStrategy = atomic rule
                    return (statementStrategy, M.insert statementId statementStrategy strategyMap)

                firstNextId : restNextIds -> do -- process all possible choices
                    firstStrategyTuple <- makeSubStrategy (tree, t) scriptId strategyMap firstNextId
                    (foldedStrategy, nextsStrategyMap) <- foldM folder firstStrategyTuple restNextIds
                    let statementStrategy = rule <*> foldedStrategy
                    return (statementStrategy, M.insert statementId statementStrategy nextsStrategyMap)

    where folder (stratSoFar, rulesSoFar) nextId = do
            (newStrat, newRules) <- makeSubStrategy (tree, t) scriptId rulesSoFar nextId
            return (stratSoFar <|> newStrat, newRules)
