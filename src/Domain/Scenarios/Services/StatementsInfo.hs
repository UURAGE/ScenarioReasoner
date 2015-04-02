{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
module Domain.Scenarios.Services.StatementsInfo where

import Data.Maybe
import Data.Char

import Ideas.Common.Library
import Ideas.Service.Types

import Domain.Scenarios.Parser
import Domain.Scenarios.Types(ID, errorOnFail, toIdTypeSegment)
          
type StatementText = (Either String [(String, String)])
tStatementText :: Type a StatementText
tStatementText = tString :|: (tList (tPair tString tString))

data StatementInfo = StatementInfo ID
                                   String
                                   StatementText
                                   [String]
                                   (Maybe String)
                                   MediaInfo

tStatementInfo :: Type a StatementInfo
tStatementInfo = 
    Iso ((<-!) pairify) (Pair (Tag "id"        tString)
                        (Pair (Tag "type"      tString)
                        (Pair (Tag "text"      tStatementText)
                        (Pair (Tag "intents"  (tList tString))
                        (Pair (Tag "feedback" (tMaybe tString))
                              (Tag "media"     tMediaInfo))))))

        where pairify (StatementInfo id tp text ints fb media) = (id, (tp, (text, (ints, (fb, media)))))


data MediaInfo = MediaInfo [(String, String)] [String]

tMediaInfo :: Type a MediaInfo
tMediaInfo = 
    Iso ((<-!) pairify) (Pair (Tag "visuals" (tList (tPair tString tString)))
                              (Tag "audios"  (tList tString)))
        where pairify (MediaInfo visuals audios) = (visuals, audios)
        
statementsinfo :: [Script] -> Exercise a -> [StatementInfo]
statementsinfo scripts ex = map statementInfo $ emptyOnFail $ getScriptStatements script
    where script = findScript "get info for" scripts ex
          statementInfo statement = StatementInfo
                (show $ createFullId script statement)
                (toIdTypeSegment $ fromJust $ getType statement)
                (either Left (Right . map showConversationTextTypeStringTuple) $
                    errorOnFail $ getText statement)
                (emptyOnFail $ getIntents statement)
                (errorOnFail $ getFeedback statement)
                (MediaInfo
                    (emptyOnFail $ getMediaVisuals statement)
                    (emptyOnFail $ getMediaAudios statement)
                )
          emptyOnFail = fromMaybe []
          showConversationTextTypeStringTuple (ctt, s) = (map toLower $ show ctt, s)


