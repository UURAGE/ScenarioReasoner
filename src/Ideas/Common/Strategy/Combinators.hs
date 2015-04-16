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
-- A collection of strategy combinators: all lifted to work on different
-- data types
--
-----------------------------------------------------------------------------
--  $Id: Combinators.hs 7524 2015-04-08 07:31:15Z bastiaan $

module Ideas.Common.Strategy.Combinators where

import Data.Array
import Data.Graph
import Data.List ((\\))
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Abstract
import Ideas.Common.Strategy.Core
import Ideas.Common.Utils (fst3)
import Prelude hiding (not, repeat, fail, sequence)
import qualified Prelude

-----------------------------------------------------------
--- Strategy combinators

-- Basic combinators --------------------------------------

infixr 2 <%>, <@>
infixr 3 <|>
infixr 4 >|>, |>
infixr 5 <*>

-- | Put two strategies in sequence (first do this, then do that)
(<*>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(<*>) = liftCore2 (:*:)

-- | Choose between the two strategies (either do this or do that)
(<|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(<|>) = liftCore2 (:|:)

-- | Interleave two strategies
(<%>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(<%>) = liftCore2 (:%:)

-- | Alternate two strategies
(<@>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(<@>) = liftCore2 (:@:)

-- | The strategy that always succeeds (without doing anything)
succeed :: Strategy a
succeed = fromCore Succeed

-- | The strategy that always fails
fail :: Strategy a
fail = fromCore Fail

-- | Makes a strategy atomic (w.r.t. parallel composition)
atomic :: IsStrategy f => f a -> Strategy a
atomic = liftCore Atomic

-- | Puts a list of strategies into a sequence
sequence :: IsStrategy f => [f a] -> Strategy a
sequence = foldr ((<*>) . toStrategy) succeed

-- | Combines a list of alternative strategies
alternatives :: IsStrategy f => [f a] -> Strategy a
alternatives = foldr ((<|>) . toStrategy) fail

-- | Merges a list of strategies (in parallel)
interleave :: IsStrategy f => [f a] -> Strategy a
interleave = foldr ((<%>) . toStrategy) succeed

-- | Allows all permutations of the list
permute :: IsStrategy f => [f a] -> Strategy a
permute = foldr ((<%>) . atomic) succeed

-- EBNF combinators --------------------------------------

-- | Repeat a strategy zero or more times (non-greedy)
many :: IsStrategy f => f a -> Strategy a
many a = fix $ \x -> succeed <|> (a <*> x)

-- | Apply a certain strategy at least once (non-greedy)
many1 :: IsStrategy f => f a -> Strategy a
many1 s = s <*> many s

-- | Apply a strategy a certain number of times
replicate :: IsStrategy f => Int -> f a -> Strategy a
replicate n = sequence . Prelude.replicate n

-- | Apply a certain strategy or do nothing (non-greedy)
option :: IsStrategy f => f a -> Strategy a
option s = s <|> succeed

-- Negation and greedy combinators ----------------------

-- | Checks whether a predicate holds for the current term. The
--   check is considered to be a minor step.
check :: (a -> Bool) -> Strategy a
check = toStrategy . checkRule "check"

-- | Check whether or not the argument strategy cannot be applied: the result
--   strategy only succeeds if this is not the case (otherwise it fails).
not :: IsStrategy f => f a -> Strategy a
not = liftCore Not

-- | Repeat a strategy zero or more times (greedy version of 'many')
repeat :: IsStrategy f => f a -> Strategy a
repeat a = fix $ \x -> (a <*> x) |> succeed

-- | Apply a certain strategy at least once (greedy version of 'many1')
repeat1 :: IsStrategy f => f a -> Strategy a
repeat1 s = s <*> repeat s

-- | Apply a certain strategy if this is possible (greedy version of 'option')
try :: IsStrategy f => f a -> Strategy a
try s = s |> succeed

-- | Choose between the two strategies, with a preference for steps from the
-- left hand-side strategy.
(>|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(>|>) = liftCore2 (:>|>)

-- | Left-biased choice: if the left-operand strategy can be applied, do so. Otherwise,
--   try the right-operand strategy
(|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
(|>) = liftCore2 (:|>:)
-- s |> t = s <|> (not s <*> t)

-- | Repeat the strategy as long as the predicate holds
while :: IsStrategy f => (a -> Bool) -> f a -> Strategy a
while p s = repeat (check p <*> s)

-- | Repeat the strategy until the predicate holds
until :: IsStrategy f => (a -> Bool) -> f a -> Strategy a
until p = while (Prelude.not . p)

-- | Apply a strategy at least once, but collapse into a single step
multi :: (IsId l, IsStrategy f) => l -> f a -> Strategy a
multi l = collapse . label l . repeat1

-- | Apply the strategies from the list exhaustively (until this is no longer possible)
exhaustive :: IsStrategy f => [f a] -> Strategy a
exhaustive = repeat . alternatives

-- | A fix-point combinator on strategies (to model recursion). Powerful
-- (but dangerous) combinator
fix :: (Strategy a -> Strategy a) -> Strategy a
fix f = fromCore (coreFix (toCore . f . fromCore))

remove :: IsStrategy f => f a -> Strategy a
remove = liftCore Remove

collapse :: IsStrategy f => f a -> Strategy a
collapse = liftCore Collapse

hide :: IsStrategy f => f a -> Strategy a
hide = liftCore Hide

-- Graph to strategy ----------------------

type DependencyGraph node key = (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

-- | Create a strategy from a dependency graph with strategies as nodes
-- Does not check for cycles
dependencyGraph:: IsStrategy f => DependencyGraph (f a) key -> Strategy a
dependencyGraph (graph, vertex2data, _) = g2s []
    where
        g2s seen
            | null reachables   = succeed
            | otherwise         = alternatives $ map makePath reachables
            where
               reachables      = filter isReachable $ vertices graph \\ seen
               isReachable     = null . (\\ seen) . (graph!)
               makePath vertex = (fst3 . vertex2data) vertex <*> g2s (vertex:seen)