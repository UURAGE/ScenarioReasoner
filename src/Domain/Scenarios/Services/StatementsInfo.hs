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
statementsinfo scripts ex = map statementInfo (getScriptStatements script)
  where script = parseScript (findScript "get info for" scripts ex)
        statementInfo statement = StatementInfo
            (show $ createFullId script statement)
            (toIdTypeSegment $ statType statement)
            (either Left (Right . map showConversationTextTypeStringTuple) $
                statText statement)
            (statIntents statement)
            (statFeedback statement)
            ((\(Media vs as) -> MediaInfo vs as) (statMedia statement))              
        showConversationTextTypeStringTuple (ctt, s) = (map toLower $ show ctt, s)

        getScriptStatements :: Script -> [Statement]
        getScriptStatements (Script _ dialogue) =  concatMap treeStatements allTrees
          where 
            allTrees = concatMap (\(_, trees) -> trees) dialogue


