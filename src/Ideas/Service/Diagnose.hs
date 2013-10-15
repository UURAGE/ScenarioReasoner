{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Diagnose a term submitted by a student
--
-----------------------------------------------------------------------------
module Ideas.Service.Diagnose
   ( Diagnosis(..), diagnose, restartIfNeeded, newState
   ) where

import Data.List (sortBy)
import Data.Maybe
import Ideas.Common.Library hiding (ready)
import Ideas.Service.BasicServices hiding (apply)
import Ideas.Service.State
import Ideas.Service.Types

----------------------------------------------------------------
-- Result types for diagnose service

data Diagnosis a
   = Buggy          Environment (Rule (Context a))
--   | Missing
--   | IncorrectPart  [a]
   | NotEquivalent
   | Similar        Bool (State a)
   | Expected       Bool (State a) (Rule (Context a))
   | Detour         Bool (State a) Environment (Rule (Context a))
   | Correct        Bool (State a)

instance Show (Diagnosis a) where
   show diagnosis =
      case diagnosis of
         Buggy as r        -> "Buggy rule " ++ show (show r) ++ showArgs as
--         Missing          -> "Missing solutions"
--         IncorrectPart xs -> "Incorrect parts (" ++ show (length xs) ++ " items)"
         NotEquivalent    -> "Unknown mistake"
         Similar _ _      -> "Very similar"
         Expected _ _ r   -> "Rule " ++ show (show r) ++ ", expected by strategy"
         Detour _ _ _ r   -> "Rule " ++ show (show r) ++ ", not following strategy"
         Correct _ _      -> "Unknown step"
    where
      showArgs as
         | noBindings as = ""
         | otherwise     = " (" ++ show as ++ ")"

newState :: Diagnosis a -> Maybe (State a)
newState diagnosis =
   case diagnosis of
      Buggy _ _        -> Nothing
      NotEquivalent    -> Nothing
      Similar  _ s     -> Just s
      Expected _ s _   -> Just s
      Detour   _ s _ _ -> Just s
      Correct  _ s     -> Just s

----------------------------------------------------------------
-- The diagnose service

diagnose :: State a -> Context a -> Diagnosis a
diagnose state new
   -- Is the submitted term equivalent?
   | not (equivalence ex (stateContext state) new) =
        -- Is the rule used discoverable by trying all known buggy rules?
        case discovered True of
           Just (r, as) -> Buggy as r -- report the buggy rule
           Nothing      -> NotEquivalent -- compareParts state new

   -- Was the submitted term expected by the strategy?
   | isJust expected =
        -- If yes, return new state and rule
        let ((r, _, _), ns) = fromJust expected
        in Expected (ready ns) ns r

   -- Is the submitted term (very) similar to the previous one?
   | similar = Similar (ready state) state

   -- Is the rule used discoverable by trying all known rules?
   | otherwise =
        case discovered False of
           Just (r, as) ->  -- If yes, report the found rule as a detour
              Detour (ready restarted) restarted as r
           Nothing -> -- If not, we give up
              Correct (ready restarted) restarted
 where
   ex        = exercise state
   restarted = restartIfNeeded (makeNoState ex new)
   similar   = similarity ex (stateContext state) new

   expected = do
      let xs = either (const []) id $ allfirsts (restartIfNeeded state)
          p (_, ns) = similarity ex new (stateContext ns) -- use rule recognizer?
      listToMaybe (filter p xs)

   discovered searchForBuggy = listToMaybe
      [ (r, env)
      | r <- sortBy (ruleOrdering ex) (ruleset ex)
      , isBuggy r == searchForBuggy
      , (_, env) <- recognizeRule ex r sub1 sub2
      ]
    where
      diff = if searchForBuggy then difference else differenceEqual
      (sub1, sub2) = fromMaybe (stateContext state, new) $ do
         newTerm <- fromContext new
         (a, b)  <- diff ex (stateTerm state) newTerm
         return (inContext ex a, inContext ex b)

----------------------------------------------------------------
-- Helpers

-- If possible (and if needed), restart the strategy
-- Make sure that the new state has a prefix
-- When resetting the prefix, also make sure that the context is refreshed
restartIfNeeded :: State a -> State a
restartIfNeeded state
   | null (statePrefixes state) && canBeRestarted ex =
        emptyState ex (stateTerm state)
   | otherwise = state
 where
   ex = exercise state

instance Typed a (Diagnosis a) where
   typed = Tag "Diagnosis" $ Iso (f <-> g) typed
    where
      f (Left (Left (as, r))) = Buggy as r
   --   f (Left (Right (Left ()))) = Missing
   --   f (Left (Right (Right (Left xs)))) = IncorrectPart xs
      f (Left (Right ())) = NotEquivalent
      f (Right (Left (b, s))) = Similar b s
      f (Right (Right (Left (b, s, r)))) = Expected b s r
      f (Right (Right (Right (Left (b, s, as, r))))) = Detour b s as r
      f (Right (Right (Right (Right (b, s))))) = Correct b s

      g (Buggy as r)       = Left (Left (as, r))
   --   g Missing            = Left (Right (Left ()))
   --   g (IncorrectPart xs) = Left (Right (Right (Left xs)))
      g NotEquivalent      = Left (Right ())
      g (Similar b s)      = Right (Left (b, s))
      g (Expected b s r)   = Right (Right (Left (b, s, r)))
      g (Detour b s as r)  = Right (Right (Right (Left (b, s, as, r))))
      g (Correct b s)      = Right (Right (Right (Right (b, s))))

----------------------------------------------------------------
-- Compare answer sets (and search for missing parts/incorrect parts)
{-  splitParts     :: a -> [a]
compareParts :: State a -> a -> Diagnosis a
compareParts state = answerList eq split solve (stateTerm state)
 where
   ex    = exercise (exercise state)
   eq    = equivalence ex
   split = splitParts ex
   solve = \a -> fromMaybe a $
                    apply (strategy ex) (inContext ex a) >>= fromContext

answerList :: (a -> a -> Bool) -> (a -> [a]) -> (a -> a) -> a -> a -> Diagnosis a
answerList eq split solve a b
   | noSplit               = NotEquivalent
   | present && null wrong = NotEquivalent -- error situation
   | null wrong            = Missing
   | partly                = IncorrectPart wrong
   | otherwise             = NotEquivalent
 where
   as = split (solve a) -- expected
   ps = [ (x, split (solve x)) | x <- split b ] -- student (keep original parts)
   bs = concatMap snd ps -- student answer, but then fully solved
   wrong   = [ x | (x, xs) <- ps, any notInAs xs ] -- is a (student) part incorrect?
   present = all (flip any bs . eq) as -- are all expected answers present
   notInAs = not . flip any as . eq
   partly  = length wrong < length ps
   noSplit = length as < 2 && length bs < 2 -}