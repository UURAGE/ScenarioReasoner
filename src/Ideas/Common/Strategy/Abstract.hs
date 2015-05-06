{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
--  $Id: Abstract.hs 7638 2015-04-30 13:23:05Z bastiaan $

module Ideas.Common.Strategy.Abstract
   ( Strategy, IsStrategy(..)
   , LabeledStrategy, label, unlabel
   , derivationList
   , emptyPrefix, replayPath, replayPaths, replayStrategy
   , rulesInStrategy
   , mapRules, mapRulesS
   , cleanUpStrategy, cleanUpStrategyAfter
     -- Accessors to the underlying representation
   , toCore, fromCore, liftCore, liftCore2, liftCoreN
   ) where

import Data.Foldable (toList)
import Data.Function
import Ideas.Common.Classes
import Ideas.Common.Derivation
import Ideas.Common.Environment
import Ideas.Common.Id
import Ideas.Common.Rewriting (RewriteRule)
import Ideas.Common.Rule
import Ideas.Common.CyclicTree hiding (label)
import Ideas.Common.Strategy.Def
import Ideas.Common.Strategy.Prefix
import Ideas.Common.Strategy.Process
import Ideas.Common.Strategy.Sequence (firstsOrdered, Sequence(..))
import Ideas.Common.Strategy.Step
import Ideas.Common.View
import Prelude hiding (sequence)
import qualified Ideas.Common.CyclicTree as Tree

-----------------------------------------------------------
--- Strategy data-type

-- | Abstract data type for strategies
newtype Strategy a = S (Core a)

instance Show (Strategy a) where
   show = show . toCore

instance Apply Strategy where
   applyAll = runCore . toCore

sequenceDef :: Def
sequenceDef = associativeDef "sequence" sequence
   
-----------------------------------------------------------
--- Type class

-- | Type class to turn values into strategies
class IsStrategy f where
   toStrategy :: f a -> Strategy a

instance IsStrategy Strategy where
   toStrategy = id

instance IsStrategy (LabeledStrategy) where
  toStrategy (LS info (S core)) = S (Tree.label info core)

instance IsStrategy Rule where
   toStrategy = S . leaf

instance IsStrategy RewriteRule where
   toStrategy = toStrategy . ruleRewrite

-----------------------------------------------------------
--- Labeled Strategy data-type

-- | A strategy which is labeled with an identifier
data LabeledStrategy a = LS Id (Strategy a)

instance Show (LabeledStrategy a) where
   show s = showId s ++ ": " ++ show (unlabel s)

instance Apply LabeledStrategy where
   applyAll = applyAll . toStrategy

instance HasId (LabeledStrategy a) where
   getId (LS l _)      = l
   changeId f (LS l s) = LS (changeId f l) s

-- | Labels a strategy with an identifier. Labels are used to identify
-- substrategies and to specialize feedback messages. The first argument of
-- 'label' can be of type 'String', in which case the string is used as
-- identifier (and not as description).
label :: (IsId l, IsStrategy f) => l -> f a -> LabeledStrategy a
label l = LS (newId l) . toStrategy

-- | Removes the label from a strategy
unlabel :: LabeledStrategy a -> Strategy a
unlabel (LS _ s) = s

-- | Construct the empty prefix for a labeled strategy
emptyPrefix :: IsStrategy f => f a -> a -> Prefix a
emptyPrefix = makePrefix . toCore

-- | Construct a prefix for a path and a labeled strategy. The third argument
-- is the current term.
replayPath :: IsStrategy f => Path -> f a -> a -> ([Step a], Prefix a)
replayPath path s a =
   let (xs, f) = replayCore path (toCore s)
   in (xs, f a)

-- | Construct a prefix for a list of paths and a labeled strategy. The third
-- argument is the current term.
replayPaths :: IsStrategy f => [Path] -> f a -> a -> Prefix a
replayPaths paths s a = mconcat
   [ snd (replayPath path s a) | path <- paths ]

-- | Construct a prefix for a path and a labeled strategy. The third argument
-- is the initial term.
replayStrategy :: (Monad m, IsStrategy f) => Path -> f a -> a -> m (a, Prefix a)
replayStrategy path s a =
   let (xs, f) = replayCore path (toCore s)
   in case applyList xs a of
         Just b  -> return (b, f b)
         Nothing -> fail "Cannot replay strategy"

-----------------------------------------------------------
--- Remaining functions

derivationList :: IsStrategy f => (Rule a -> Rule a -> Ordering) -> f a -> a -> [Derivation (Rule a, Environment) a]
derivationList cmpRule s a0 = rec a0 (toPrefix s)
 where
   toPrefix = majorPrefix . flip makePrefix a0 . toCore

   rec a prfx = (if ready prfx then (emptyDerivation a:) else id)
      [ prepend (a, rEnv) d | (rEnv, b, new) <- firstsOrd prfx, d <- rec b new ]

   firstsOrd = map f . firstsOrdered cmp
    where
      cmp = cmpRule `on` (fst . g . fst)

      f ((stp, b), new) = (g stp, b, new)

      g stp = (stepRule stp, stepEnvironment stp)

-- | Returns a list of all major rules that are part of a labeled strategy
rulesInStrategy :: IsStrategy f => f a -> [Rule a]
rulesInStrategy s = [ r | r <- toList (toCore s), isMajor r ]

instance LiftView LabeledStrategy where
   liftViewIn = mapRules . liftViewIn

instance LiftView Strategy where
   liftViewIn = mapRulesS . liftViewIn

-- | Apply a function to all the rules that make up a labeled strategy
mapRules :: (Rule a -> Rule b) -> LabeledStrategy a -> LabeledStrategy b
mapRules f (LS n s) = LS n (mapRulesS f s)

mapRulesS :: (Rule a -> Rule b) -> Strategy a -> Strategy b
mapRulesS f = S . fmap f . toCore

-- | Use a function as do-after hook for all rules in a labeled strategy, but
-- also use the function beforehand
cleanUpStrategy :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategy f (LS n s) = cleanUpStrategyAfter f (LS n t)
 where
   t     = doAfter f (idRule ()) <*> s
   (<*>) = liftCore2 (node2 sequenceDef) -- fix me

-- | Use a function as do-after hook for all rules in a labeled strategy
cleanUpStrategyAfter :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategyAfter f = mapRules $ \r ->
   if isMajor r then doAfter f r else r

-----------------------------------------------------------
--- Functions to lift the core combinators

toCore :: IsStrategy f => f a -> Core a
toCore s = let S core = toStrategy s in core

fromCore :: Core a -> Strategy a
fromCore = S

liftCore :: IsStrategy f => (Core a -> Core a) -> f a -> Strategy a
liftCore f = fromCore . f . toCore

liftCore2 :: (IsStrategy f, IsStrategy g) => (Core a -> Core a -> Core a) -> f a -> g a -> Strategy a
liftCore2 f = liftCore . f . toCore

liftCoreN :: IsStrategy f => ([Core a] -> Core a) -> [f a] -> Strategy a
liftCoreN f = fromCore . f . map toCore