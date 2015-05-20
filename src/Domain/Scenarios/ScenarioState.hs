{-# LANGUAGE FlexibleInstances, OverlappingInstances, AutoDeriveTypeable #-} 
module Domain.Scenarios.ScenarioState where

import Control.Monad
import Control.Applicative((<*>))
import Data.Char

import qualified Data.Map as M

import Ideas.Common.Library hiding ((<*>))
import Ideas.Common.Utils 
import Ideas.Text.JSON
import Data.Typeable

import Domain.Scenarios.Globals

-- | ScenarioState
-- The state is affected by every step (rule / statement) that has an effect in a strategy.
data ScenarioState = ScenarioState ParameterMap EmotionMap StatementInfo 
    deriving (Eq, Typeable)

instance Show ScenarioState where
    show (ScenarioState pmap emap end) = show pmap ++ show emap ++ show end
  
type ParameterMap = M.Map ID ParameterValue
type EmotionMap = M.Map Emotion ParameterValue

-- | The effect of a statement on the current state
data Effect = Effect
        { effectIdref      :: ID
        , effectChangeType :: ChangeType
        , effectValue      :: ParameterValue
        } 
        
instance Show Effect where
    show (Effect id ct value) = "\n\t\t" ++ show id ++ show ct ++ show value
    
-- This datatype specifies the type of change to be made to the parameter, 
-- Set for setting the parameter to a specific value
-- Delta for adding / subtracting the new value to / from the existing value     
data ChangeType = Set | Delta deriving (Show, Eq, Read)

applyEffects :: ScenarioState -> [Effect] -> [Effect] -> StatementInfo -> ScenarioState
applyEffects (ScenarioState paramMap emotionMap _) paramEffects emotionEffects statInfo = 
    ScenarioState (foldr applyEffect paramMap paramEffects) (foldr applyEffect emotionMap emotionEffects) statInfo

-- | Applies the chosen effect to the state
applyEffect :: Effect -> M.Map String ParameterValue -> M.Map String ParameterValue
applyEffect effect stateMap = case effectChangeType effect of
        Set   -> setParam idref value stateMap
        Delta -> setParam idref ((getParamOrZero idref stateMap) + value) stateMap
    where idref = effectIdref effect
          value = effectValue effect          
          
-- Functions for changing the ScenarioState 

-- If the parameter is in the state return its value otherwise return zero
getParamOrZero :: String -> M.Map String ParameterValue -> ParameterValue
getParamOrZero pID state = M.findWithDefault 0 pID state

-- Set the parameter to a specific value and return the new state
setParam :: String -> ParameterValue -> M.Map String ParameterValue -> M.Map String ParameterValue
setParam pID value state = M.insert pID value state
          

-- ScenarioState to JSON for sending and receiving a Map datatype in JSON ---------------------------

instance InJSON ScenarioState where
    toJSON (ScenarioState params emos stat) = Object [parametersToJSON, emotionsToJSON, statInfoToJSON]
        where
            emotionsToJSON = ("emotions", toJSON emos)
            parametersToJSON  =  ("parameters", toJSON params)
            statInfoToJSON = ("statement", toJSON stat)
    fromJSON (Object (("parameters", paramsJSON) : ("emotions", emotionsJSON) : ("statement", statInfoJSON) : [])) = 
        do params <- fromJSON paramsJSON
           emotions <- fromJSON emotionsJSON
           statInfo <- fromJSON statInfoJSON
           return (ScenarioState params emotions statInfo)
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
        intentsToJSON   = ("intentions",   toJSON (statIntents     statInfo))
        feedbackToJSON  = ("feedback",  toJSON (statFeedback    statInfo))
        mediaToJSON     = ("media",     toJSON (statMedia       statInfo))
        endToJSON       = ("end",       toJSON (statEnd         statInfo))
    fromJSON (Object (typeTuple : textTuple : intentsTuple : feedbackTuple : mediaTuple : endTuple : [])) = do
        statType <- (\("type", typeJSON) -> fromJSON typeJSON) typeTuple
        text     <- (\("text", textJSON) -> fromJSON textJSON) textTuple
        intents  <- (\("intentions", intentsJSON) -> fromJSON intentsJSON) intentsTuple
        feedback <- (\("feedback", feedbackJSON) -> fromJSON feedbackJSON) feedbackTuple
        media    <- (\("media", mediaJSON) -> fromJSON mediaJSON) mediaTuple
        end      <- (\("end", endJSON) -> fromJSON endJSON) endTuple
        return (StatementInfo statType text intents feedback media end)
    fromJSON _ = fail "expecting an object"
    
instance InJSON StatementText where
    toJSON (Left string) = listToJSON string
    toJSON (Right conversation) = toJSON conversation
    toJSON _ = error "wrong conversationtext"
    fromJSON stringJSON@(String _) = return (Left (fromJSON stringJSON))
    fromJSON listJSON@(Array _)    = return (Right (fromJSON listJSON))
    fromJSON _    = fail "expecting a string or a list of tuples"
    
instance InJSON MediaInfo where
    toJSON (MediaInfo visuals audios) = Object [("visuals", toJSON visuals), ("audios", toJSON audios)]
    fromJSON (Object (("visuals", visualsJSON) : ("audios", audiosJSON) : [])) =
        return (MediaInfo (fromJSON visualsJSON) (fromJSON audiosJSON))
    fromJSON _                     = fail "expecting an object"
    
instance InJSON (Maybe String) where
    toJSON (Just string) = toJSON string 
    toJSON Nothing = Null
    
    fromJSON stringJSON@(String _) = fromJSON stringJSON
    fromJSON (Null)                = return Nothing
    fromJSON _                     = fail "expecting a string or null"
    
showJSON :: ScenarioState -> String
showJSON = compactJSON . toJSON

readJSON :: String -> Either String ScenarioState
readJSON = either Left (maybe (Left "failed to interpret JSON state") Right . fromJSON) . parseJSON

-- Instances of isTerm
--   toTerm   :: a -> Term
--   fromTerm :: MonadPlus m => Term -> m a

instance IsTerm ScenarioState where
    toTerm (ScenarioState paramMap emotionMap statInfo) = toTerm (paramMap, toTerm (emotionMap, statInfo))
    fromTerm term = do
        (params, emotionAndStatTerm) <- fromTerm term 
        (emotions, statInfo) <- fromTerm emotionAndStatTerm
        return (ScenarioState params emotions statInfo)
    

instance IsTerm (M.Map ID ParameterValue) where
    toTerm = toTerm . M.toAscList . (M.mapKeysMonotonic ShowString)
    fromTerm x = do 
        x' <- fromTerm x
        return (M.mapKeysMonotonic fromShowString (M.fromDistinctAscList x'))
   
instance IsTerm StatementInfo where
    toTerm statInfo = 
        toTerm [toTerm (statType statInfo), toTerm (statText statInfo), toTerm (statIntents statInfo), toTerm (statFeedback statInfo), toTerm (statMedia statInfo), toTerm (statEnd statInfo)]
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
   
----------------------------------------------------------------------------------------------------