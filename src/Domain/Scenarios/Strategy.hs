module Domain.Scenarios.Strategy where 

import Control.Monad
import Data.List
import qualified Data.Map as M

import Ideas.Common.Library

import Domain.Scenarios.Types
import Domain.Scenarios.Parser

type StrategyMap a = M.Map String (Strategy a)

--framework code, try not to break your head.
guardedRule :: IsId b => b -> String -> (a -> Bool) -> (a -> a) -> Rule a
guardedRule identifier description check update =
    describe description $ makeRule identifier (\x -> do guard $ check x; Just $ update x)
    --make description and ad it to the rule $ make the rule

makeStrategy :: Monad m => Script -> m (Strategy State)
makeStrategy script = do
    startId <- getScriptStartId script
    trees <- getTrees script
    scriptId <- getScriptId script
    (fullStrategy, _) <- makeSubStrategy (head (head trees)) scriptId M.empty startId
    return fullStrategy

--sub strats make a strat for one tree, so it should not be to hard to expand it once we know the starting statements of each tree.
makeSubStrategy :: Monad m => (Tree, TreeElement) -> String -> StrategyMap State -> String -> m (Strategy State, StrategyMap State)
makeSubStrategy (tree,  t@(TreeElement el)) scriptId strategyMap statementId = do

    let statement = head $ findStatementAt el statementId
    statementType <- getType statement
    statementDescription <- getText statement
    statementPrecondition <- getMaybePrecondition statement
    statementEffects <- getEffects statement

    case M.lookup statementId strategyMap of --check if statment is already in the strategy
        
        Just statementStrategy -> return (statementStrategy, strategyMap) --if it is already in the strategy do nothing and just return the strategy

        Nothing -> do
            let rule = guardedRule
                    (["scenarios", scriptId, toIdTypeSegment statementType, statementId])
                    (either id (intercalate " // " . map snd) statementDescription)
                    (calculateMaybeCondition statementPrecondition)
                    (\state -> foldr applyEffect state statementEffects)
            nextIds <- getNexts statement

            case nextIds of

                []                        -> do
                    let statementStrategy = atomic rule
                    return (statementStrategy, M.insert statementId statementStrategy strategyMap)

                firstNextId : restNextIds -> do
                    firstStrategyTuple <- makeSubStrategy (tree, t) scriptId strategyMap firstNextId
                    (foldedStrategy, nextsStrategyMap) <- foldM folder firstStrategyTuple restNextIds
                    let statementStrategy = rule <*> foldedStrategy
                    return (statementStrategy, M.insert statementId statementStrategy nextsStrategyMap)

    where folder (stratSoFar, rulesSoFar) nextId = do
            (newStrat, newRules) <- makeSubStrategy (tree, t) scriptId rulesSoFar nextId
            return (stratSoFar <|> newStrat, newRules) 
