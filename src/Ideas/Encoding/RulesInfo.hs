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
--  $Id: RulesInfo.hs 8740 2015-10-14 19:26:22Z bastiaan $

module Ideas.Encoding.RulesInfo
   ( rulesInfoXML, rewriteRuleToFMP, collectExamples, ExampleMap
   ) where

import Data.Char
import Ideas.Common.Library
import Ideas.Common.Utils (Some(..))
import Ideas.Encoding.OpenMathSupport (toOMOBJ)
import Ideas.Text.OpenMath.FMP
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML hiding (name)
import qualified Data.Map as M

rulesInfoXML :: Exercise a -> (a -> XMLBuilder) -> XMLBuilder
rulesInfoXML ex enc = mconcat (map ruleInfoXML (ruleset ex))
 where
   ruleInfoXML r = element "rule"
      [ "name"        .=. showId r
      , "buggy"       .=. f (isBuggy r)
      , "rewriterule" .=. f (isRewriteRule r)
        -- More information
      , let descr = description r
            -- to do: rules should carry descriptions
            txt   = if null descr then showId r else descr
        in munless (null txt) $
              tag "description" $ string txt
      , mconcat [ tag "argument" (text a) | Some a <- getRefs r ]
      , mconcat [ tag "sibling" $ text s | s <- ruleSiblings r ]
      -- FMPs and CMPs
      , mconcat [  case showRewriteRule ok rr of
                      Nothing -> mempty
                      Just s  -> tag "CMP" (string s)
                <> tag "FMP" (builder (omobj2xml (toObject fmp)))
                | Some rr <- getRewriteRules (transformation r)
                , let ok  = not $ isBuggy r
                , let fmp = rewriteRuleToFMP ok rr
                ]
      -- Examples
      , mconcat [ element "example" [enc a, enc b]
                | let pairs = M.findWithDefault [] (getId r) (collectExamples ex)
                , (a, b) <- take 3 pairs
                ]
      ]
   f = map toLower . show

rewriteRuleToFMP :: Bool -> RewriteRule a -> FMP
rewriteRuleToFMP sound r
   | sound     = eqFMP    a b
   | otherwise = buggyFMP a b
 where
   a :~> b = fmap toOMOBJ (ruleSpecTerm r)

type ExampleMap a = M.Map Id [(a, a)]

collectExamples :: Exercise a -> ExampleMap a
collectExamples ex = foldr (add . snd) M.empty (examples ex)
 where
   add a m = let f = foldr g m . maybe [] triples
                 g (x, (r, _), y) =
                    case fromContextWith2 (,) x y of
                       Just p  -> M.insertWith (++) (getId r) [p]
                       Nothing -> id
             in f (defaultDerivation ex a)