{-# LANGUAGE FlexibleInstances, AutoDeriveTypeable, TypeSynonymInstances #-} 
module Domain.Scenarios.ScenarioState where

import Control.Monad
import Data.Char

import qualified Data.Map as M

import Ideas.Common.Library
import Ideas.Common.Utils 
import Ideas.Text.JSON
import Data.Typeable

import Domain.Scenarios.Globals(ID, ParameterValue, Emotion)

-- | ScenarioState
-- The state is affected by every step (rule / statement) that has an effect in a strategy.
data ScenarioState = ScenarioState ParameterMap EmotionMap
    deriving (Eq, Typeable)

instance Show ScenarioState where
    show (ScenarioState pmap emap) = show pmap ++ show emap

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

applyEffects :: ScenarioState -> [Effect] -> [Effect] -> ScenarioState
applyEffects (ScenarioState paramMap emotionMap) paramEffects emotionEffects = 
    ScenarioState (foldr applyEffect paramMap paramEffects) (foldr applyEffect emotionMap emotionEffects)

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
    toJSON (ScenarioState params emos) = Object [parametersToJSON params, emotionsToJSON emos]
        where
            emotionsToJSON emotions = ("emotions", toJSON emotions)
            parametersToJSON parameters =  ("parameters", toJSON parameters)
    fromJSON (Object (("parameters", paramsJSON) : ("emotions", emotionsJSON):[])) = 
        do params <- fromJSON paramsJSON
           emotions <- fromJSON emotionsJSON
           return (ScenarioState params emotions)

instance InJSON a => InJSON (M.Map String a)  where
    toJSON = Object . map kvpToJSON . M.assocs
        where kvpToJSON (key, value) = (key, toJSON value)
    fromJSON (Object kjvps) = liftM M.fromList (mapM kvpFromJSON kjvps)
        where kvpFromJSON (key, jvalue) = liftM2 (,) (return key) (fromJSON jvalue) 
    fromJSON _ = fail "expecting an object"

showJSON :: ScenarioState -> String
showJSON = compactJSON . toJSON

readJSON :: String -> Either String ScenarioState
readJSON = either Left (maybe (Left "failed to interpret JSON state") Right . fromJSON) . parseJSON

-- Instances of isTerm
--   toTerm   :: a -> Term
--   fromTerm :: MonadPlus m => Term -> m a

instance IsTerm ScenarioState where
    toTerm (ScenarioState paramMap emotionMap) = toTerm (paramMap, emotionMap)
    fromTerm term = do
        statePair <- fromTerm term 
        return (ScenarioState (fst statePair) (snd statePair))
    

instance IsTerm (M.Map ID ParameterValue) where
 toTerm = toTerm . M.toAscList . (M.mapKeysMonotonic ShowString)
 fromTerm x = do 
   x' <- fromTerm x
   return (M.mapKeysMonotonic fromShowString (M.fromDistinctAscList x'))
   
----------------------------------------------------------------------------------------------------