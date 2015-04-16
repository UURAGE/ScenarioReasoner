{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
module Domain.Scenarios.ScriptState where

import Control.Monad
import Data.Char

import qualified Data.Map as M

import Ideas.Common.Library
import Ideas.Common.Utils 
import Ideas.Text.JSON

import Domain.Scenarios.Globals(ID, ParameterValue)

-- | ScriptState
-- The state is affected by every step (rule / statement) that has an effect in a strategy.
type ScriptState = (M.Map ID ParameterValue, ID)

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

-- | Applies the chosen effect to the state
applyEffect :: Effect -> ScriptState -> ScriptState
applyEffect effect state = case effectChangeType effect of
        Set   -> setParam idref value state
        Delta -> setParam idref ((getParamOrZero idref state) + value) state
    where idref = effectIdref effect
          value = effectValue effect
          

-- ScriptState to JSON for sending and receiving a Map datatype in JSON ---------------------------

instance InJSON a => InJSON (M.Map String a)  where
    toJSON = Object . map kvpToJSON . M.assocs
        where kvpToJSON (key, value) = (key, toJSON value)
    fromJSON (Object kjvps) = liftM M.fromList (mapM kvpFromJSON kjvps)
        where kvpFromJSON (key, jvalue) = liftM2 (,) (return key) (fromJSON jvalue) 
    fromJSON _ = fail "expecting an object"

showJSON :: ScriptState -> String
showJSON = compactJSON . toJSON

readJSON :: String -> Either String ScriptState
readJSON = either Left (maybe (Left "failed to interpret JSON state") Right . fromJSON) . parseJSON

-- Instances of isTerm
--   toTerm   :: a -> Term
--   fromTerm :: MonadPlus m => Term -> m a

instance IsTerm (M.Map ID ParameterValue) where
 toTerm = toTerm . M.toAscList . (M.mapKeysMonotonic ShowString)
 fromTerm x = do 
   x' <- fromTerm x
   return (M.mapKeysMonotonic fromShowString (M.fromDistinctAscList x'))
   
---------------------------------------------------------------------------------------------------

-- Functions for changing the ScriptState 

-- If the parameter is in the state return its value otherwise return zero
getParamOrZero :: ID -> ScriptState -> ParameterValue
getParamOrZero pID state = M.findWithDefault 0 pID (fst state)

-- Set the parameter to a specific value
setParam :: ID -> ParameterValue -> ScriptState -> ScriptState
setParam pID value state = (M.insert pID value (fst state), snd state)