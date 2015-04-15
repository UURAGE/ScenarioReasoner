{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
module Domain.Scenarios.Id where

import Data.Char(isLower, toLower)
import Data.List(intercalate)

import Ideas.Common.Library

import Domain.Scenarios.Globals(ScriptElement, applyToFirst)
import Domain.Scenarios.Parser

-- Definitions of Id instances and functions (Id is a data structure from the Ideas framework)
---------------------------------------------------------

instance HasId ScriptElement where
    getId scriptElem = either error id $ do
                let id = parseScriptID scriptElem
                let descr = parseScriptDescription scriptElem
                return $ describe descr $ "scenarios" # id
    changeId _ _ = error "The ID of a ScriptElement is determined externally."

instance HasId Script where
    getId (Script metadata _) = either error id $ do
                let id = scriptID metadata
                let descr = scriptDescription metadata
                return $ describe descr $ "scenarios" # id
    changeId _ _ = error "The ID of a ScriptElement is determined externally."

instance HasId Statement where
    getId statement = either error id $ do
                let statementId = statID statement
                let statementText = statText statement
                let statementDescription = either id (intercalate " // " . map snd) statementText
                return $ describe statementDescription $ newId statementId
    changeId _ _ = error "The ID of a Statement is determined externally."
    

-- | Creates the full ID for the given statement in the context of the given script.
createFullId :: Script -> Statement -> Id
createFullId script statement = scriptId # typeSegment # statId # interleaveSegment
  where 
    scriptId = getId script
    typeSegment = toIdTypeSegment $ statType statement
    statId = statID statement 
    
    nextIDs = nextStatIDs statement
    
    interleaveSegment | jumpPoint statement                                 = "interleaved"
                      | not (endOfConversation statement) && (null nextIDs) = "interleaved"
                      | otherwise                                           = ""

-- | Returns the value to be used to represent a statement type in a rule ID.
toIdTypeSegment :: StatementType -> String
toIdTypeSegment = takeWhile isLower . applyToFirst toLower . show                    
                      
findScript :: String -> [ScriptElement] -> Exercise a -> ScriptElement
findScript usage scripts ex =
    case filter (\testScript -> (getId testScript) == (getId ex)) scripts of
            [foundScript] -> foundScript
            _             ->
                error $ "Cannot " ++ usage ++ " exercise: exercise is apparently not a Scenario."