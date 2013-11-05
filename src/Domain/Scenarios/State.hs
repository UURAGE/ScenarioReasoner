{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
module Domain.Scenarios.State where

import Ideas.Common.Library
import Ideas.Common.Utils
import Ideas.Text.JSON
import Control.Monad

import qualified Data.Map as M

{-- 
The state is affected by every step in a strategy.
--}

---------------------------
-- State

type State = M.Map String Int

-- State to JSON

instance InJSON a => InJSON (M.Map String a)  where
    toJSON = Object . map kvpToJSON . M.assocs
        where kvpToJSON (key, value) = (key, toJSON value)
    fromJSON (Object kjvps) = liftM M.fromList (mapM kvpFromJSON kjvps)
        where kvpFromJSON (key, jvalue) = liftM2 (,) (return key) (fromJSON jvalue) 
    fromJSON _ = fail "expecting an object"

showJSON :: State -> String
showJSON = showCompact . toJSON

readJSON :: String -> Either String State
readJSON = either Left (maybe (Left "failed to interpret JSON state") Right . fromJSON) . parseJSON

-- - -
-- Instances of isTerm
--   toTerm   :: a -> Term
--   fromTerm :: MonadPlus m => Term -> m a
-- - -

instance IsTerm (M.Map String Int) where
 toTerm = toTerm . M.toAscList . (M.mapKeysMonotonic ShowString)
 fromTerm x = do 
   x' <- fromTerm x
   return (M.mapKeysMonotonic fromShowString (M.fromDistinctAscList x'))

setZero, setOne :: String -> State -> State
setZero = flip M.insert 1
setOne = flip M.insert 1

onlyOne :: String -> State -> State
onlyOne s = (flip M.insert 1 s).(M.map (\_-> 0))

isZero, isOne :: String -> State -> Bool
isZero s cf = isVal 0 True s (cf)
isOne s cf = isVal 1 False s (cf)

isEmpty :: State -> Bool
isEmpty cf = M.null (cf)

eitherIsOne :: [String] -> State -> Bool
eitherIsOne [] _      = False
eitherIsOne (x:xs) ck = (isOne x ck) || (eitherIsOne xs ck)

isVal :: Int -> Bool -> String -> State -> Bool
isVal v d s m = case M.lookup s m of
 Nothing -> d
 Just x -> (x == v)
 
andF :: (a->Bool) -> (a->Bool) -> a -> Bool
andF f g a = (f a) && (g a)

orF :: (a->Bool) -> (a->Bool) -> a -> Bool
orF f g a = (f a) || (g a)

emptyState :: State
emptyState = M.empty
