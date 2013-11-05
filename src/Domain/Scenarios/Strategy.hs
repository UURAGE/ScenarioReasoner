module Domain.Scenarios.Strategy where 

import Ideas.Common.Library
import Domain.Scenarios.State
import Domain.Scenarios.Parser
import Control.Monad

guardedRule :: IsId b => b -> String -> (a -> Bool) -> (a -> a) -> Rule a
guardedRule identifier description check update = describe description $ makeRule identifier (\x -> do guard $ check x; Just $ update x)

makeStrategy :: Monad m => Script -> m (Strategy State)
makeStrategy script = do
    startId <- getScriptStartId script
    makeSubStrategy script startId

makeSubStrategy :: Monad m => Script -> String -> m (Strategy State)
makeSubStrategy script statementId = do
    scriptId <- getScriptId script
    statement <- getStatement script statementId
    statementDescription <- getText statement
    let rule = guardedRule
            ("scenarios." ++ scriptId ++ "." ++ statementId)
            statementDescription
            (const true)
            id
    nextIds <- getNexts statement
    nexts <- mapM (makeSubStrategy script) nextIds
    return $ if null nexts then atomic rule else rule <*> (foldr1 (<|>) nexts)