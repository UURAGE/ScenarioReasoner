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
-- Locations in a strategy
--
-----------------------------------------------------------------------------
--  $Id: Location.hs 7638 2015-04-30 13:23:05Z bastiaan $

module Ideas.Common.Strategy.Location
   ( checkLocation, subTaskLocation, nextTaskLocation
   , strategyLocations
   ) where

import Data.Maybe
import Ideas.Common.Id
import Ideas.Common.Strategy.Abstract
import Ideas.Common.CyclicTree

-----------------------------------------------------------
--- Strategy locations

checkLocation :: Id -> LabeledStrategy a -> Bool
checkLocation loc =
   any ((==loc) . getId . snd) . strategyLocations

-- old (current) and actual (next major rule) location
subTaskLocation :: LabeledStrategy a -> Id -> Id -> Id
subTaskLocation s xs ys = g (rec (f xs) (f ys))
 where
   f = fromMaybe [] . toLoc s
   g = fromMaybe (getId s) . fromLoc s
   rec (i:is) (j:js)
      | i == j    = i : rec is js
      | otherwise = []
   rec _ (j:_)    = [j]
   rec _ _        = []

-- old (current) and actual (next major rule) location
nextTaskLocation :: LabeledStrategy a -> Id -> Id -> Id
nextTaskLocation s xs ys = g (rec (f xs) (f ys))
 where
   f = fromMaybe [] . toLoc s
   g = fromMaybe (getId s) . fromLoc s
   rec (i:is) (j:js)
      | i == j    = i : rec is js
      | otherwise = [j]
   rec _ _        = []

-- | Returns a list of all strategy locations, paired with the label
strategyLocations :: LabeledStrategy a -> [([Int], Id)]
strategyLocations s = ([], getId s) : make s
 where
   make = nrs . fold alg . toCore . unlabel
   alg  = monoidAlg 
      { fLeaf  = \a   -> [(getId a, [])]
      , fLabel = \l x -> [(l, nrs x)]
      }
   nrs  = concat . zipWith f [0..]

   f i (l, xs) = ([i], l) : [ (i:is, l2) | (is, l2) <- xs ]

fromLoc :: LabeledStrategy a -> [Int] -> Maybe Id
fromLoc s loc = fmap getId (lookup loc (strategyLocations s))

toLoc :: LabeledStrategy a -> Id -> Maybe [Int]
toLoc s i =
   fmap fst (listToMaybe (filter ((==i) . getId . snd) (strategyLocations s)))