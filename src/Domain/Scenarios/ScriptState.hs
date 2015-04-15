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
-- The state is affected by every step in a strategy.

type ScriptState = (M.Map ID ParameterValue, ID)

-- | The effect of a statement on the current state
data Effect = Effect
        { effectIdref      :: ID
        , effectChangeType :: ChangeType
        , effectValue      :: ParameterValue
        } deriving (Show, Eq)
data ChangeType = Set | Delta deriving (Show, Eq, Read)

-- | Applies the chosen effect to the state
applyEffect :: Effect -> ScriptState -> ScriptState
applyEffect effect state = case effectChangeType effect of
        Set   -> setParam idref value state
        Delta -> setParam idref ((getParamOrZero idref state) + value) state
    where idref = effectIdref effect
          value = effectValue effect

-- ScriptState to JSON
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

-- - -
-- Instances of isTerm
--   toTerm   :: a -> Term
--   fromTerm :: MonadPlus m => Term -> m a
-- - -

instance IsTerm (M.Map ID ParameterValue) where
 toTerm = toTerm . M.toAscList . (M.mapKeysMonotonic ShowString)
 fromTerm x = do 
   x' <- fromTerm x
   return (M.mapKeysMonotonic fromShowString (M.fromDistinctAscList x'))

getParamOrZero :: ID -> ScriptState -> ParameterValue
getParamOrZero name state = M.findWithDefault 0 name (fst state)

setZero, setOne :: ID -> ScriptState -> ScriptState
setZero name state = (flip M.insert 1 name (fst state), snd state)
setOne name state = (flip M.insert 1 name (fst state), snd state)

setParam :: ID -> ParameterValue -> ScriptState -> ScriptState
setParam name value state = (M.insert name value (fst state), snd state)

onlyOne :: ID -> ScriptState -> ScriptState
onlyOne name state = ((flip M.insert 1 name).(M.map (\_-> 0)) $ (fst state), snd state)

isZero, isOne :: ID -> ScriptState -> Bool
isZero s cf = isVal 0 True s (cf)
isOne s cf = isVal 1 False s (cf)

isEmpty :: ScriptState -> Bool
isEmpty cf = M.null (fst cf)

eitherIsOne :: [ID] -> ScriptState -> Bool
eitherIsOne [] _      = False
eitherIsOne (x:xs) ck = (isOne x ck) || (eitherIsOne xs ck)

isVal :: ParameterValue -> Bool -> ID -> ScriptState -> Bool
isVal v d s m = case M.lookup s (fst m) of
 Nothing -> d
 Just x -> (x == v)