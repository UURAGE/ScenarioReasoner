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
--  $Id: Symbol.hs 8740 2015-10-14 19:26:22Z bastiaan $

module Ideas.Text.OpenMath.Symbol where

type Symbol = (Maybe String, String)

-- * Constructor functions

makeSymbol :: String -> String -> Symbol
makeSymbol = (,) . Just

extraSymbol :: String -> Symbol
extraSymbol = (,) Nothing

-- * Selector functions

dictionary :: Symbol -> Maybe String
dictionary = fst

symbolName :: Symbol -> String
symbolName = snd

-- * Utility function

showSymbol :: Symbol -> String
showSymbol s = maybe "" (++".") (dictionary s) ++ symbolName s