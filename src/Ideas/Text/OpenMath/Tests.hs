-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
--  $Id: Tests.hs 8740 2015-10-14 19:26:22Z bastiaan $

module Ideas.Text.OpenMath.Tests (propEncoding) where

import Control.Monad
import Ideas.Text.OpenMath.Dictionary.Arith1
import Ideas.Text.OpenMath.Dictionary.Calculus1
import Ideas.Text.OpenMath.Dictionary.Fns1
import Ideas.Text.OpenMath.Dictionary.Linalg2
import Ideas.Text.OpenMath.Dictionary.List1
import Ideas.Text.OpenMath.Dictionary.Logic1
import Ideas.Text.OpenMath.Dictionary.Nums1
import Ideas.Text.OpenMath.Dictionary.Quant1
import Ideas.Text.OpenMath.Dictionary.Relation1
import Ideas.Text.OpenMath.Dictionary.Transc1
import Ideas.Text.OpenMath.Object
import Test.QuickCheck

arbOMOBJ :: Gen OMOBJ
arbOMOBJ = sized rec
 where
   symbols = arith1List ++ calculus1List ++ fns1List ++ linalg2List ++
      list1List ++ logic1List ++ nums1List ++ quant1List ++
      relation1List ++ transc1List

   rec 0 = frequency
      [ (1, liftM OMI arbitrary)
      , (1, liftM (\n -> OMF (fromInteger n / 1000)) arbitrary)
      , (1, liftM OMV arbitrary)
      , (5, elements $ map OMS symbols)
      ]
   rec n = frequency
      [ (1, rec 0)
      , (3, choose (1,4) >>= liftM OMA . (`replicateM` f))
      , (1, liftM3 OMBIND f arbitrary f)
      ]
    where
      f = rec (n `div` 2)

propEncoding :: Property
propEncoding = forAll arbOMOBJ $ \x -> xml2omobj (omobj2xml x) == Right x