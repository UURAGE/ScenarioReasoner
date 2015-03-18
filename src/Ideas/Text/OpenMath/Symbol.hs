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
--  $Id: Symbol.hs 6535 2014-05-14 11:05:06Z bastiaan $

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