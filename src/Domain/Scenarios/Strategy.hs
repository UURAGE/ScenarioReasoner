module Domain.Scenarios.Strategy where 

import Ideas.Common.Library
import Domain.Scenarios.Types
import Domain.Scenarios.Parser
import Control.Monad
import qualified Data.Map as M

type StrategyMap a = M.Map String (Strategy a)

guardedRule :: IsId b => b -> String -> (a -> Bool) -> (a -> a) -> Rule a
guardedRule identifier description check update =
    describe description $ makeRule identifier (\x -> do guard $ check x; Just $ update x)

makeStrategy :: Monad m => Script -> m (Strategy State)
makeStrategy script = do
    startId <- getScriptStartId script
    (fullStrategy, _) <- makeSubStrategy script M.empty startId
    return fullStrategy

makeSubStrategy :: Monad m => Script -> StrategyMap State -> String -> m (Strategy State, StrategyMap State)
makeSubStrategy script strategyMap statementId = do
    scriptId <- getScriptId script
    statement <- findStatement script statementId
    statementDescription <- getText statement
    case M.lookup statementId strategyMap of
        Just statementStrategy -> return (statementStrategy, strategyMap)
        Nothing -> do
            let rule = guardedRule
                    ("scenarios." ++ scriptId ++ "." ++ statementId)
                    (statementDescription)
                    (const true)
                    (id)
            nextIds <- getNexts statement
            case nextIds of
                []                        -> do
                    let statementStrategy = atomic rule
                    return (statementStrategy, M.insert statementId statementStrategy strategyMap)
                firstNextId : restNextIds -> do
                    firstStrategyTuple <- makeSubStrategy script strategyMap firstNextId
                    (foldedStrategy, nextsStrategyMap) <- foldM folder firstStrategyTuple restNextIds
                    let statementStrategy = rule <*> foldedStrategy
                    return (statementStrategy, M.insert statementId statementStrategy nextsStrategyMap)
    where folder (stratSoFar, rulesSoFar) nextId = do
            (newStrat, newRules) <- makeSubStrategy script rulesSoFar nextId
            return (stratSoFar <|> newStrat, newRules) 
