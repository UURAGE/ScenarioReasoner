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
-- A strategy is a context-free grammar with rules as symbols. Strategies can be
-- labeled with strings. A type class is introduced to lift all the combinators
-- that work on strategies, only to prevent that you have to insert these lifting
-- functions yourself.
--
-----------------------------------------------------------------------------
--  $Id: Strategy.hs 7524 2015-04-08 07:31:15Z bastiaan $

module Ideas.Common.Strategy
   ( -- * Data types and type classes
     Strategy, LabeledStrategy
   , IsStrategy(..)
     -- * Running strategies
   , derivationList
     -- * Strategy combinators
     -- ** Basic combinators
   , (<*>), (<|>), (<%>), succeed, fail, atomic, label
   , sequence, alternatives, interleave, permute, fix
     -- ** EBNF combinators
   , many, many1, replicate, option
     -- ** Negation and greedy combinators
   , check, not, repeat, repeat1, try, (|>), (>|>), exhaustive
   , while, until, multi
     -- ** Graph
   , DependencyGraph, dependencyGraph
     -- ** Traversal combinators
   , module Ideas.Common.Strategy.Traversal
     -- * Configuration combinators
   , module Ideas.Common.Strategy.Configuration
   , remove, collapse, hide
     -- * Strategy locations
   , strategyLocations, checkLocation
   , subTaskLocation, nextTaskLocation
     -- * Prefixes
   , Prefix, emptyPrefix, noPrefix
   , replayPath, replayPaths, replayStrategy
   , Step(..), stepRule, stepEnvironment
   , Path, emptyPath, readPath, readPaths
   , prefixPaths, majorPrefix, isEmptyPrefix
     -- * Misc
   , cleanUpStrategy, cleanUpStrategyAfter
   , rulesInStrategy
   ) where

import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Combinators
import Ideas.Common.Strategy.Configuration
import Ideas.Common.Strategy.Location
import Ideas.Common.Strategy.Parsing
import Ideas.Common.Strategy.Traversal hiding (full, spine, stop, once)
import Prelude ()