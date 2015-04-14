{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Domain.Scenarios.Services.StatementsInfo where

import Data.Maybe
import Data.Char

import Ideas.Common.Library

import Domain.Scenarios.Parser
import Domain.Scenarios.TypeDefs(ID, Name)
          
type StatementText = (Either String [(String, String)])

data StatementInfo = StatementInfo ID
                                   String         -- type
                                   StatementText
                                   [String]       -- intentions
                                   String         -- feedback
                                   MediaInfo

data MediaInfo = MediaInfo [(Name, ID)] [ID] -- MediaInfo Video Audio
        
statementsinfo :: [ScriptElem] -> Exercise a -> [StatementInfo]
statementsinfo scripts ex = map statementInfo getScriptStatements script
    where script = findScript "get info for" scripts ex
          statementInfo statement = StatementInfo
                (show $ createFullId script statement)
                (toIdTypeSegment $ statType statement)
                (either Left (Right . map showConversationTextTypeStringTuple) $
                    statText statement)
                (statIntents statement)
                (statFeedback statement)
                (statMedia statement)                
          showConversationTextTypeStringTuple (ctt, s) = (map toLower $ show ctt, s)
          
getScriptStatements :: ScriptElem -> [Statement]
getScriptStatements scriptElem = concatMap treeStatements allTrees
  where 
    dialogue = parseDialogue scriptElem
    allTrees = map (\(_, trees) -> (++) trees) dialogue


