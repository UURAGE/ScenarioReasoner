{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
--  $Id: SmartGroup.hs 6535 2014-05-14 11:05:06Z bastiaan $

module Ideas.Common.Algebra.SmartGroup
   ( -- * Smart datatypes
     Smart(..), SmartZero(..), SmartGroup(..)
     --- * Smart field
   , SmartField(..), (.+.), (.-.), neg, (.*.), (./.)
     -- * Smart booleans
   , (.&&.), (.||.)
   ) where

import Control.Applicative
import Control.Monad (mplus)
import Data.Maybe
import Ideas.Common.Algebra.Boolean
import Ideas.Common.Algebra.Field hiding ((<*>))
import Ideas.Common.Algebra.Group
import qualified Ideas.Common.Algebra.Field as Field

newtype Smart a = Smart {fromSmart :: a}
   deriving (Show, Eq, Ord, CoMonoid, MonoidZero, CoMonoidZero)

instance Functor Smart where -- could be derived
   fmap f = Smart . f . fromSmart

instance Applicative Smart where
   pure = Smart
   Smart f <*> Smart a = Smart (f a)

instance (CoMonoid a, Monoid a) => Monoid (Smart a) where
   mempty = Smart mempty
   mappend a b
      | isEmpty a = b
      | isEmpty b = a
      | otherwise = liftA2 (<>) a b

--------------------------------------------------------------

newtype SmartZero a = SmartZero {fromSmartZero :: a}
   deriving (Show, Eq, Ord, MonoidZero, CoMonoid, CoMonoidZero)

instance Functor SmartZero where -- could be derived
   fmap f = SmartZero . f . fromSmartZero

instance Applicative SmartZero where
   pure = SmartZero
   SmartZero f <*> SmartZero a = SmartZero (f a)

instance (CoMonoidZero a, MonoidZero a) => Monoid (SmartZero a) where
   mempty = SmartZero mempty
   mappend a b
      | isMonoidZero a || isMonoidZero b = mzero
      | otherwise = liftA2 (<>) a b

--------------------------------------------------------------

newtype SmartGroup a = SmartGroup {fromSmartGroup :: a}
   deriving (Show, Eq, Ord, CoMonoid, CoGroup, CoMonoidZero, MonoidZero)

instance Functor SmartGroup where -- could be derived
   fmap f = SmartGroup . f . fromSmartGroup

instance Applicative SmartGroup where
   pure = SmartGroup
   SmartGroup f <*> SmartGroup a = SmartGroup (f a)

instance (CoGroup a, Group a) => Monoid (SmartGroup a) where
   mempty  = SmartGroup mempty
   mappend a b
      | isEmpty a = b
      | otherwise = fromMaybe (liftA2 (<>) a b) (matchGroup alg b)
    where
      alg = (a, \x y -> (a <> x) <> y, \x -> a <>- x, \x y -> (a <> x) <>- y)

instance (CoGroup a, Group a) => Group (SmartGroup a) where
   inverse a = fromMaybe (liftA inverse a) (matchGroup alg a)
    where
      alg = (mempty, \x y -> inverse x <>- y, id, \x y -> inverse x <> y)
   appendInv a b
      | isEmpty a = inverse b
      | otherwise = fromMaybe (liftA2 (<>-) a b) (matchGroup alg b)
    where
      alg = (a, \x y -> (a <>- x) <>- y, \x -> a <> x, \x y -> (a <>- x) <> y)

--------------------------------------------------------------

type GroupMatch a b = (b, a -> a -> b, a -> b, a -> a -> b)

matchGroup :: CoGroup a => GroupMatch a b -> a -> Maybe b
matchGroup (emp, app, inv, appinv) a =
   (if isEmpty a then Just emp else Nothing) `mplus`
   fmap (uncurry app) (isAppend a)  `mplus`
   fmap inv (isInverse a) `mplus`
   fmap (uncurry appinv) (isAppendInv a)

--------------------------------------------------------------
-- Smart Field

newtype SmartField a = SmartField {fromSmartField :: a}
   deriving (CoSemiRing, CoRing, CoField)

instance Functor SmartField where -- could be derived
   fmap f = SmartField . f . fromSmartField

instance Applicative SmartField where
   pure = SmartField
   SmartField f <*> SmartField a = SmartField (f a)

instance (CoField a, Field a) => SemiRing (SmartField a) where
   zero = SmartField zero
   one  = SmartField one
   SmartField a <+> SmartField b = SmartField $ fromAdditive $ fromSmartGroup $
      SmartGroup (Additive a) <> SmartGroup (Additive b)
   a <*> b
      | Just x <- isNegate a = plusInverse (x Field.<*> b)
      | Just x <- isNegate b = plusInverse (a Field.<*> x)
      | isZero a || isZero b = zero
      | isOne a = b
      | isOne b = a
      | Just (x, y) <- isTimes b = (a Field.<*> x) Field.<*> y
      | Just (x, y) <- isDivision b = (a Field.<*> x) </> y
      | otherwise = liftA2 (Field.<*>) a b

instance (CoField a, Field a) => Ring (SmartField a) where
   plusInverse = SmartField . fromAdditive . fromSmartGroup . inverse
               . SmartGroup . Additive . fromSmartField
   SmartField a <-> SmartField b = SmartField $ fromAdditive $ fromSmartGroup $
      SmartGroup (Additive a) <>- SmartGroup (Additive b)

instance (CoField a, Field a) => Field (SmartField a) where
   timesInverse a
      | Just x <- isNegate a = plusInverse (timesInverse x)
      | Just (x, y) <- isDivision a, isOne y = x
      | otherwise = liftA timesInverse a
   a </> b
      | Just x <- isNegate a = plusInverse (x </> b)
      | Just x <- isNegate b = plusInverse (a </> x)
      | isOne b = a
      | Just (x, y) <- isDivision a = x </> (y Field.<*> b)
      | otherwise = liftA2 (</>) a b

------------------------------------------------------------------

infixl 7 .*., ./.
infixl 6 .-., .+.

(.+.) :: (CoField a, Field a) => a -> a -> a
a .+. b = fromSmartField $ SmartField a <+> SmartField b

(.-.) :: (CoField a, Field a) => a -> a -> a
a .-. b = fromSmartField $ SmartField a <-> SmartField b

neg :: (CoField a, Field a) => a -> a
neg = fromSmartField . plusInverse . SmartField

(.*.) :: (CoField a, Field a) => a -> a -> a
a .*. b = fromSmartField $ SmartField a Field.<*> SmartField b

(./.) :: (CoField a, Field a) => a -> a -> a
a ./. b = fromSmartField $ SmartField a </> SmartField b

-- myrecip :: (CoField a, Field a) => a -> a
-- myrecip = fromSmartField . timesInverse . SmartField

--------------------------------------------------------------
-- Smart booleans

instance BoolValue a => BoolValue (Smart a) where
   fromBool = Smart   . fromBool
   isTrue   = isTrue  . fromSmart
   isFalse  = isFalse . fromSmart

instance (Boolean a, CoBoolean a) => Boolean (Smart a) where
   a <&&> b = fmap fromAnd $ fromSmartZero $
      SmartZero (fmap And a) <> SmartZero (fmap And b)
   a <||> b = fmap fromOr $ fromSmartZero $
      SmartZero (fmap Or a) <> SmartZero (fmap Or b)
   complement (Smart a)
      | isTrue  a = false
      | isFalse a = true
      | otherwise = Smart $ fromMaybe (complement a) (isComplement a)

infixr 4 .||.
infixr 5 .&&.

(.&&.), (.||.) :: (Boolean a, CoBoolean a) => a -> a -> a
a .&&. b = fromSmart $ Smart a <&&> Smart b
a .||. b = fromSmart $ Smart a <||> Smart b