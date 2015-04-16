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
--  $Id: Recognizer.hs 7524 2015-04-08 07:31:15Z bastiaan $

module Ideas.Common.Rule.Recognizer
   ( -- * data type and type class
     Recognizable(..), Recognizer
     -- * Constructor functions
   , makeRecognizer, makeRecognizerEnvMonad, makeRecognizerTrans
   ) where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Ideas.Common.Environment
import Ideas.Common.Rule.EnvironmentMonad
import Ideas.Common.Rule.Transformation
import Ideas.Common.View

-----------------------------------------------------------
--- Data type and type class

class Recognizable f where
   recognizer     :: f a -> Recognizer a
   recognizeAll   :: f a -> a -> a -> [Environment]
   recognize      :: f a -> a -> a -> Maybe Environment
   recognizeTrans :: f a -> Trans (a, a) ()
   -- default definitions
   recognizeAll r a b = map snd $ transApply (recognizeTrans r) (a, b)
   recognize    r a b = listToMaybe $ recognizeAll r a b
   recognizeTrans     = unR . recognizer

newtype Recognizer a = R { unR :: Trans (a, a) () }

instance LiftView Recognizer where
   liftViewIn v r =
      let f = fmap fst . match v
      in R $ makeTrans f *** makeTrans f >>> unR r

instance Monoid (Recognizer a) where
   mempty      = R mempty
   mappend f g = R $ unR f `mappend` unR g

instance Recognizable Recognizer where
   recognizer = id

instance HasRefs (Recognizer a) where
   allRefs = allRefs . unR

-----------------------------------------------------------
--- Constructor functions

makeRecognizer :: (a -> a -> Bool) -> Recognizer a
makeRecognizer eq = makeRecognizerEnvMonad $ \a b -> guard (eq a b)

makeRecognizerEnvMonad :: (a -> a -> EnvMonad ()) -> Recognizer a
makeRecognizerEnvMonad = makeRecognizerTrans . makeTrans . uncurry

makeRecognizerTrans :: Trans (a, a) () -> Recognizer a
makeRecognizerTrans = R