{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, DeriveGeneric #-} 

------------------------------------------------------------------------------------
-- This program has been developed by students from the bachelor Computer Science
-- at Utrecht University within the Software and Game project course (2013-2015)
-- ©Copyright Utrecht University (Department of Information and Computing Sciences)
------------------------------------------------------------------------------------

module Domain.Scenarios.ScenarioState where

import Control.Monad

import qualified Data.Map as M
import Data.Typeable
import Data.Binary

import Ideas.Common.Library
import Ideas.Common.Utils 
import Ideas.Text.JSON

import GHC.Generics

import Domain.Scenarios.Globals

-- | ScenarioState
-- The state is affected by every step (rule / statement) that has an effect in a strategy.
data ScenarioState = ScenarioState ParameterMap StatementInfo
    deriving (Show, Eq, Typeable, Read, Generic)

instance Binary ScenarioState

type ParameterMap = M.Map ID ParameterValue

-- | The effect of a statement on the current state
data Effect = Effect
        { effectIdref      :: ID
        , effectChangeType :: ChangeType
        , effectValue      :: ParameterValue
        } 
 deriving (Show, Read, Generic)

instance Binary Effect

-- This datatype specifies the type of change to be made to the parameter,
-- Set for setting the parameter to a specific value
-- Delta for adding / subtracting the new value to / from the existing value
data ChangeType = Set | Delta deriving (Show, Eq, Read, Generic)

instance Binary ChangeType

applyEffects :: ScenarioState -> [Effect] -> StatementInfo -> ScenarioState
applyEffects (ScenarioState paramMap _) paramEffects statInfo = 
    ScenarioState (foldr applyEffect paramMap paramEffects) statInfo

-- | Applies the chosen effect to the state
applyEffect :: Effect -> M.Map String ParameterValue -> M.Map String ParameterValue
applyEffect effect stateMap = case effectChangeType effect of
        Set   -> M.insert idref value stateMap
        Delta -> M.insert idref (M.findWithDefault 0 idref stateMap + value) stateMap
    where idref = effectIdref effect
          value = effectValue effect


-- ScenarioState to JSON for sending and receiving datatypes in JSON ---------------------------

instance InJSON ScenarioState where
    toJSON (ScenarioState params stat) = Object [parametersToJSON, statInfoToJSON]
        where
            parametersToJSON  =  ("parameters", toJSON params)
            statInfoToJSON = ("statement", toJSON stat)
    fromJSON (Object (("parameters", paramsJSON) : ("statement", _) : [])) = 
        do params <- fromJSON paramsJSON
           return (ScenarioState params emptyStatementInfo)
    fromJSON _ = fail "expecting an object"

instance InJSON a => InJSON (M.Map String a)  where
    toJSON = Object . map kvpToJSON . M.assocs
        where kvpToJSON (key, value) = (key, toJSON value)
    fromJSON (Object kjvps) = liftM M.fromList (mapM kvpFromJSON kjvps)
        where kvpFromJSON (key, jvalue) = liftM2 (,) (return key) (fromJSON jvalue) 
    fromJSON _ = fail "expecting an object"
    
instance InJSON StatementInfo  where
    toJSON statInfo = Object [typeToJSON, textToJSON, intentsToJSON, feedbackToJSON, mediaToJSON, endToJSON]
      where 
        typeToJSON      = ("type",      toJSON (statType        statInfo))
        textToJSON      = ("text",      toJSON (statText        statInfo))
        intentsToJSON   = ("intentions", toJSON (statIntents     statInfo))
        feedbackToJSON  = ("feedback",  toJSON (statFeedback    statInfo))
        mediaToJSON     = ("media",     toJSON (statMedia       statInfo))
        endToJSON       = ("end",       toJSON (statEnd         statInfo))
    fromJSON _ = fail "expecting an object"
    
instance InJSON (Either String [(String, String)]) where
    toJSON (Left text) = toJSON text
    toJSON (Right conversation) = toJSON conversation
    fromJSON _ = fail "fail: the response is a default value in the statement info"
    
instance InJSON MediaInfo where
    toJSON (MediaInfo visuals audios) = Object [("visuals", toJSON visuals), ("audios", toJSON audios)]
    fromJSON _ = fail "fail: the response is a default value in the statement info"
    
instance InJSON (Maybe String) where
    toJSON (Just string) = toJSON string 
    toJSON Nothing = Null
    fromJSON _ = fail "fail: the response is a default value in the statement info"
    
showJSON :: ScenarioState -> String
showJSON = compactJSON . toJSON

readJSON :: String -> Either String ScenarioState
readJSON = either Left (maybe (Left "failed to interpret JSON state") Right . fromJSON) . parseJSON

-- Instances of isTerm
--   toTerm   :: a -> Term
--   fromTerm :: MonadPlus m => Term -> m a

instance IsTerm ScenarioState where
    toTerm (ScenarioState paramMap statInfo) = toTerm (paramMap, statInfo)
    fromTerm term = do
        (params, statInfo) <- fromTerm term
        return (ScenarioState params statInfo)
    

instance IsTerm (M.Map ID ParameterValue) where
    toTerm = toTerm . M.toAscList . M.mapKeysMonotonic ShowString
    fromTerm x = do 
        x' <- fromTerm x
        return (M.mapKeysMonotonic fromShowString (M.fromDistinctAscList x'))
   
instance IsTerm StatementInfo where
    toTerm statInfo = 
        toTerm [toTerm (statType statInfo),     toTerm (statText statInfo)
               ,toTerm (statIntents statInfo),  toTerm (statFeedback statInfo)
               ,toTerm (statMedia statInfo),    toTerm (statEnd statInfo)]
    fromTerm statTerm = do
        statInfo <- fromTerm statTerm
        return statInfo
        
instance IsTerm MediaInfo where
    toTerm (MediaInfo visuals audios) =
        toTerm (toTerm visuals, toTerm audios)
    fromTerm mediaInfoTerm = do
        (visualsTerm, audiosTerm) <- fromTerm mediaInfoTerm
        return (MediaInfo (fromTerm visualsTerm) (fromTerm audiosTerm))
        
instance IsTerm (Maybe String) where
    toTerm (Just string) = toTerm (ShowString string)
    toTerm Nothing       = toTerm (ShowString "")
    fromTerm term = do
        sString <- fromTerm term
        let string = fromShowString sString
        return (if null string then Nothing else (Just string))
   
instance IsTerm Bool where
    toTerm True = toTerm $ ShowString "true"
    toTerm False = toTerm $ ShowString "false"
    fromTerm boolTerm = do
        bool <- fromTerm boolTerm
        return (fromShowString bool == "true")
