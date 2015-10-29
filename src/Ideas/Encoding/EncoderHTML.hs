{-# LANGUAGE GADTs #-}
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
-- Encoding in HTML
--
-----------------------------------------------------------------------------
--  $Id: EncoderHTML.hs 8745 2015-10-15 14:45:46Z bastiaan $

module Ideas.Encoding.EncoderHTML (htmlEncoder, htmlEncoderAt) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Ideas.Common.Library hiding (alternatives)
import Ideas.Common.Strategy.Symbol
import Ideas.Common.Utils
import Ideas.Common.Utils.TestSuite
import Ideas.Encoding.Encoder
import Ideas.Encoding.LinkManager
import Ideas.Encoding.RulePresenter
import Ideas.Encoding.RulesInfo
import Ideas.Encoding.StrategyInfo
import Ideas.Service.BasicServices
import Ideas.Service.Diagnose
import Ideas.Service.DomainReasoner
import Ideas.Service.Request
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Text.HTML
import Ideas.Text.OpenMath.FMP
import Ideas.Text.OpenMath.Object
import Ideas.Text.XML
import System.IO.Unsafe

type HTMLEncoder a t = Encoder a t HTMLBuilder

htmlEncoder :: DomainReasoner -> TypedEncoder a HTMLPage
htmlEncoder = htmlEncoderAt 0

htmlEncoderAt :: Int -> DomainReasoner -> TypedEncoder a HTMLPage
htmlEncoderAt n dr = do
   req <- getRequest
   let lm = f (maybe staticLinks dynamicLinks (cgiBinary req))
       f  = if n==0 then id else linksUp n
   makePage lm dr <$> encodeType lm dr

makePage :: LinkManager -> DomainReasoner -> HTMLBuilder -> HTMLPage
makePage lm dr a =
   addCSS (urlForCSS lm "ideas.css") $
   htmlPage "Ideas: documentation pages" $ mconcat
      [ divClass "page-header" $ mconcat
           [ divClass  "ideas-logo" space
           , divClass  "ounl-logo"  space
           , spanClass "menuitem"   $ linkToIndex lm $ string "Index"
           , spanClass "menuitem"   $ linkToExercises lm $ string "Exercises"
           , spanClass "menuitem"   $ linkToServices lm  $ string "Services"
           ]
      , divClass "page-content" a
      , divClass "page-footer" $
           string (fullVersion dr)
      ]

encodeType :: LinkManager -> DomainReasoner -> HTMLEncoder a (TypedValue (Type a))
encodeType lm dr =
   (encodeIndex, tDomainReasoner) <?>
   (exerciseHeader lm <> htmlDiagnosis lm dr, tDiagnosis) <?>
   (exerciseHeader lm <> encodeExampleList lm, tList (tPair tDifficulty tContext)) <?>
   (exerciseHeader lm <> htmlFirsts lm, tList (tPair tStepInfo tState)) <?>
   (exerciseHeader lm <> htmlAllApplications lm, tList (tTuple3 tRule tLocation tState)) <?>
   (exerciseHeader lm <> encodeDerivation lm, tDerivation (tPair tRule tEnvironment) tContext) <?>
   (exerciseHeader lm <> encodeDerivationList lm, tList (tDerivation (tPair tRule tEnvironment) tContext)) <?>
   encoderFor (\(val ::: tp) ->
        case tp of
           Iso iso t  -> encodeType lm dr // (to iso val ::: t)
           Tag _ t    -> encodeType lm dr // (val ::: t)
           Pair t1 t2 -> encodeType lm dr // (fst val ::: t1) <>
                         encodeType lm dr // (snd val ::: t2)
           t1 :|: t2  -> case val of
                            Left x  -> encodeType lm dr // (x ::: t1)
                            Right x -> encodeType lm dr // (x ::: t2)
           List (Const Service) -> encodeServiceList lm // val
           List (Const SomeExercise) -> encodeExerciseList lm // val
           List (Const Rule) -> exerciseHeader lm <> encodeRuleList lm // val
           List t -> ul [ encodeType lm dr // (x ::: t) | x <- val ]
           Const t -> encodeConst lm dr // (val ::: t)
           _ -> string $ "unknown: " ++ show tp)

encodeConst :: LinkManager -> DomainReasoner -> HTMLEncoder a (TypedValue (Const a))
encodeConst lm dr = encoderFor $ \tv@(val ::: tp) ->
   case tp of
      Service     -> encodeService // val
      Exercise    -> exerciseHeader lm <> encodeExercise lm // val
      Strategy    -> exerciseHeader lm <> encodeStrategy // val
      Rule        -> encodeRule // val
      State       -> exerciseHeader lm <> (encodeState lm dr // val)
      Location    -> text val
      Environment -> text val
      Context     -> encodeContext // val
      String      -> string val
      Result      -> exerciseHeader lm <> encodeResult lm val
      _           -> text tv

encodeContext :: HTMLEncoder a (Context a)
encodeContext = exerciseEncoder $ \ex ->
   string . prettyPrinterContext ex

encodeIndex :: HTMLEncoder a DomainReasoner
encodeIndex = makeEncoder $ \dr -> mconcat
   [ h1 $ "Domain reasoner " ++ showId dr
   , htmlDescription dr
   , keyValueTable
        [ ("version", string $ fullVersion dr)
        , ("exercises", text $ length $ exercises dr)
        , ("services", text $ length $ services dr)
        ]
   , munless (null $ aliases dr) $
        h2 "Exercise aliases" <>
        table True (
           [ string "alias", string "exercise"] :
           [ [string (showId a), string (showId b)]
           | (a, b) <- aliases dr
           ])
   , munless (null $ scripts dr)
        h2 "Feedback scripts" <>
        table True (
           [ string "exercise", string "script"] :
           [ [string (showId a), string file]
           | (a, file) <- scripts dr
           ])
   ]

encodeServiceList :: LinkManager -> HTMLEncoder a [Service]
encodeServiceList lm = makeEncoder $ \srvs ->
   h1 "Services" <>
   mconcat
      [ h2 (show i ++ ". " ++ s) <> table False (map make xs)
      | (i, s, xs) <- groupById srvs
      ]
 where
   make s = [ linkToService lm s (string (showId s)) <>
              mwhen (serviceDeprecated s) (italic (string " (deprecated)"))
            , string (description s)
            ]

encodeExerciseList :: LinkManager -> HTMLEncoder a [Some Exercise]
encodeExerciseList lm = makeEncoder $ \exs ->
   h1 "Exercises" <>
   mconcat
      [ h2 (show i ++ ". " ++ dom) <> table False (map make xs)
      | (i, dom, xs) <- groupsWith f exs
      ]
 where
   f :: Some Exercise -> String
   f (Some ex) = fromMaybe "" (listToMaybe (qualifiers (getId ex)))

   make :: Some Exercise -> [HTMLBuilder]
   make (Some ex) =
      [ linkToExercise lm ex $ string $ showId ex
      , string $ map toLower $ show $ status ex
      , string $ description ex
      ]

groupById :: HasId a => [a] -> [(Int, String, [a])]
groupById = groupsWith (fromMaybe "" . listToMaybe . qualifiers . getId)

groupsWith :: (a -> String) -> [a] -> [(Int, String, [a])]
groupsWith = orderedGroupsWith id

orderedGroupsWith :: Ord b => (b -> String) -> (a -> b) -> [a] -> [(Int, String, [a])]
orderedGroupsWith showf get =
   zipWith f [1..] . groupBy eq . sortBy (comparing get)
 where
   eq x y = get x == get y
   f i xs = (i, showf (get (head xs)), xs)

encodeService :: HTMLEncoder a Service
encodeService = makeEncoder $ \srv -> mconcat
   [ h1 $ "Service " ++ showId srv
   , mwhen (serviceDeprecated srv) $
        para $ spanClass "warning" $ string "Warning: this service is deprecated"
   , htmlDescription srv
   , case serviceFunction srv of
        _ ::: tp ->
           let (xs, ys) = inputOutputTypes tp
               f :: Some (Type a) -> HTMLBuilder
               f (Some (t :|: Unit)) = text t <> italic (string " (optional)")
               f (Some t) = text t
           in
              munless (null xs) (para $
                 bold (string "Input") <> ul (map f xs))
              <>
              munless (null ys) (para $
                 bold (string "Output") <> ul (map f ys))
   ]

inputOutputTypes :: Type a t -> ([Some (Type a)], [Some (Type a)])
inputOutputTypes tp =
   case tp of
      Iso _ t   -> inputOutputTypes t
      t1 :-> t2 -> let (xs, ys) = inputOutputTypes t2
                   in (productType t1 ++ xs, ys)
      Const String :|: t -> ([], productType t)
      _         -> ([], productType tp)

productType :: Type a t -> [Some (Type a)]
productType tp =
   case tp of
      Iso _ t    -> productType t
      Pair t1 t2 -> productType t1 ++ productType t2
      Unit       -> []
      _          -> [Some tp]

encodeExercise :: LinkManager -> HTMLEncoder a (Exercise a)
encodeExercise lm = makeEncoder $ \ex -> mconcat
   [ generalInfo ex
   , h2 "Example exercises"
   , ul $ [ para $ linkToExamples lm ex $ string "list of examples"
          | not (null (examples ex))
          ] ++
          [ para $ mconcat $
               string "generate exercise: " :
               intersperse (string ", ")
                  [ linkToRandomExample lm ex d $ text d
                  | d <- [VeryEasy .. VeryDifficult]
                  ]
          | isJust (randomExercise ex)
          ] ++
          [ para $ submitStateInfo lm ex
          | not (isStatic lm)
          ]
   ]
 where
   generalInfo ex = keyValueTable
      [ ("Code",   ttText (showId ex))
      , ("Status", text $ status ex)
      , ("Strategy", linkToStrategy lm ex $ string (showId $ strategy ex))
      , ("Rules", text nrOfSoundRules)
      , ("Buggy rules", text nrOfBuggyRules)
      , ("OpenMath support", bool $ isJust $ hasTermView ex)
      , ("Restartable strategy", bool $ canBeRestarted ex)
      , ("Exercise generator", bool $ isJust $ randomExercise ex)
      , ("Examples", text $ length $ examples ex)
      ]
    where
      (nrOfBuggyRules, nrOfSoundRules) =
         mapBoth length (partition isBuggy (ruleset ex))

exerciseHeader :: LinkManager -> HTMLEncoder a b
exerciseHeader lm = withExercise $ \ex -> mconcat
   [ exerciseMenu lm
   , h1 $ "Exercise " ++ showId ex
   , pure $ htmlDescription ex
   ]

exerciseMenu :: LinkManager -> HTMLEncoder a b
exerciseMenu lm = divClass "menubox" $
   bold (string "Exercise") <>
   ul [ with linkToExercise    "information"
      , with linkToStrategy "   strategy"
      , with linkToRules       "rules"
      , with linkToExamples    "examples"
      , with linkToDerivations "derivations"
      , with linkToTestReport  "test report"
      ]
 where
   with f s = withExercise $ \ex ->
      pure $ f lm ex (string s)

encodeStrategy :: HTMLEncoder a (Strategy (Context a))
encodeStrategy = exerciseEncoder $ \ex s -> mconcat
   [ h2 "Strategy"
   , highlightXML True (strategyToXML s)
   , h2 "Locations"
   , let f :: HasId a => ([Int], a) -> [HTMLBuilder]
         f (loc, a) = [text loc, indent (length loc) <> string (showId a)]
         indent n = string (replicate (3*n) '.')
     in table True
           ( [string "Location", string "Label"]
           : map f (strategyLocations (strategy ex))
           )
   ]

bool :: Bool -> HTMLBuilder
bool b = string (if b then "yes" else "no")

encodeResult :: BuildXML b => LinkManager -> Result -> b
encodeResult lm tests = mconcat
   [ h2 "Test report"
   , divClass "test-summary" $ mconcat
        [ divClass "test-status" (statusImg lm tests 32)
        , keyValueTable
             [ ("Tests",    text (nrOfTests tests))
             , ("Errors",   text (nrOfErrors tests))
             , ("Warnings", text (nrOfWarnings tests))
             , ("Time",     string (show (timeInterval tests) ++ "s"))
             , ("Rating",   showRating lm $ fromMaybe 10 $ rating tests)
             ]
        , h3 "Suites"
        , ul [ string s <> space <> text t
             | (s, t) <- subResults tests
             ]
        ]
   , mwhen (isError tests) $
        mconcat (h2 "Errors" : map makeItem errors)
   , mwhen (isWarning tests) $
        mconcat (h2 "Warnings" : map makeItem warnings)
   , h2 "Tests"
   , make tests
   ]
 where
   msgs     = allMessages tests
   errors   = filter (isError . snd) msgs
   warnings = filter (isWarning . snd) msgs

   make t = mconcat $
      map makeGroup (subResults t) ++
      map makeItem (topMessages t)

   makeGroup (s, t) = divClass "test-group" $
      divClass "test-title" (string (s ++ " " ++ show t))
      <> make t

   makeItem (s, m) = divClass "test-item" $
      statusImg lm m 16 <> spaces 3 <> string s <> msg
    where
      msg | isOk m      = mempty
          | otherwise   = string ": " <> string (intercalate "," (messageLines m))

statusImg :: (HasStatus a, BuildXML b) => LinkManager -> a -> Int -> b
statusImg lm a n = element "img"
   [ "src"    .=. urlForImage lm (statusSrc a)
   , "height" .=. show n
   , "width"  .=. show n
   ]

statusSrc :: HasStatus a => a -> String
statusSrc a
   | isError a   = "stop.png"
   | isWarning a = "flagblue.png"
   | otherwise   = "ok.png"

showRating :: BuildXML a => LinkManager -> Int -> a
showRating lm = rec (5::Int)
 where
   rec 0 _ = mempty
   rec n a = element "img"
      [ "src"    .=. urlForImage lm png
      , "height" .=. "16"
      , "width"  .=. "16"
      ] <> rec (n-1) (a-2)
    where
      png | a >= 2    = "star.png"
          | a == 1    = "star_2.png"
          | otherwise = "star_3.png"

encodeRuleList :: LinkManager -> HTMLEncoder a [Rule (Context a)]
encodeRuleList lm = exerciseEncoder $ \ex rs ->
   let (rs1, rs2) = partition isBuggy rs

       header = [ string "Rule name", string "Args"
                , string "Used", string "Siblings", string "Rewrite rule"
                ]
       used = rulesInStrategy (strategy ex)
       f r  = [ linkToRule lm ex r $ ttText (showId r)
              , text $ length $ getRefs r
              , bool $ r `elem` used
              , string $ intercalate ", " $ map show $ ruleSiblings r
              , mwhen (isRewriteRule r) $
                   ruleToHTML (Some ex) r
              ]

   in mconcat
         [ h2 $ "Rules for " ++ showId ex
         , table True (header:map f rs2)
         , h2 $ "Buggy rules for " ++ showId ex
         , table True (header:map f rs1)
         ]

encodeRule :: HTMLEncoder a (Rule (Context a))
encodeRule = exerciseEncoder $ \ex r -> mconcat
   [ h1 $ "Rule " ++ showId r
   , htmlDescription r
   , let commas  = string . intercalate ", "
         idList  = commas . map showId
         refList = commas . map show . getRefIds
     in para $ keyValueTable
           [ ("Parameters", refList r)
           , ("Buggy", bool (isBuggy r))
           , ("Rewrite rule", bool (isRewriteRule r))
           , ("Siblings", idList $ ruleSiblings r)
           ]
   , mwhen (isRewriteRule r) $
        h2 "Rewrite rule" <> ruleToHTML (Some ex) r
   -- FMPS
   , let xs = getRewriteRules (transformation r)
     in munless (null xs) $ mconcat $
           h2 "Formal Mathematical Properties" :
           [ para $
              let fmp = rewriteRuleToFMP (not $ isBuggy r) rr
              in highlightXML False $ makeXML "FMP" $
                    builder $ omobj2xml $ toObject fmp
           | Some rr <- xs
           ]
   ]

encodeExampleList :: LinkManager -> HTMLEncoder a [(Difficulty, Context a)]
encodeExampleList lm = exerciseEncoder $ \ex pairs -> mconcat $
   h2 "Examples" :
   [ h3 (s ++ " (" ++ show (length xs) ++ ")")
       <> (if isStatic lm then ul else mconcat) (map (make ex) xs)
   | (_, s, xs) <- orderedGroupsWith show fst pairs
   ]
 where
   make ex (_, x) = para $
      munless (isStatic lm) (
         let st = emptyStateContext ex x
         in spanClass "statelink" $ linkToState lm st $ external lm)
      <> spanClass "term" (string (prettyPrinterContext ex x))

external :: BuildXML a => LinkManager -> a
external lm = element "img"
   ["src" .=. urlForImage lm "external.png", "width" .=. "15"]

encodeDerivation :: LinkManager -> HTMLEncoder a (Derivation (Rule (Context a), Environment) (Context a))
encodeDerivation lm =
   h2 "Derivation" <> htmlDerivation lm

encodeDerivationList :: LinkManager -> HTMLEncoder a [Derivation (Rule (Context a), Environment) (Context a)]
encodeDerivationList lm = encoderFor $ \ds ->
   h2 "Derivations"
   <> mconcat
      [ h3 (show i ++ ".") <> htmlDerivation lm // d
      | (i, d) <- zip [1::Int ..] ds
      ]

htmlDerivation :: LinkManager -> HTMLEncoder a (Derivation (Rule (Context a), Environment) (Context a))
htmlDerivation lm = exerciseEncoder $ \ex d ->
   let before =
          stateLink lm (emptyStateContext ex (firstTerm d))
          <> case fmap (isReady ex) (fromContext (lastTerm d)) of
                Just True -> mempty
                _ -> spanClass "error" (string "Final term is not finished")
       forStep ((r, env1), env2) =
          let showEnv e = munless (noBindings e) $ string $ ", " ++ show e in
          spanClass "derivation-step" $ mconcat
             [ unescaped "&#8658; "
             , linkToRule lm ex r $ string $ showId r
             , showEnv env1 -- local environment
             , showEnv env2 -- global environment (diff)
             ]
       textLines = mconcat . intersperse br . map string . lines
       forTerm a =
          divClass "term" $ textLines $ prettyPrinterContext ex a
   in htmlDerivationWith before forStep forTerm (diffEnvironment d)

htmlState :: LinkManager -> HTMLEncoder a (State a)
htmlState lm = makeEncoder $ \state ->
   para $ divClass "state" $
      stateLink lm state
      <> divClass "term" (string $ prettyPrinterContext (exercise state) (stateContext state))
      <> string "ready: " <> bool (finished state)

stateLink :: LinkManager -> State a -> HTMLBuilder
stateLink lm st
   | isStatic lm = mempty
   | otherwise =
        spanClass "derivation-statelink" $ linkToState lm st $ external lm

encodeState :: LinkManager -> DomainReasoner -> HTMLEncoder a (State a)
encodeState lm dr =
   htmlState lm <>
   makeEncoder (\state ->
      let xs = useAllFirsts dr state
          n  = either (const 0) length xs
      in mconcat
         [ h2 "Feedback"
         , submitDiagnose lm state
         , ul [ case xs of
                   Right (hd:_) -> linkToState lm (snd hd) $ string "onefirst"
                   _ -> string "(no onefirst)"
              , linkToFirsts lm state $ string $ "allfirsts (" ++ show n ++ ")"
              , linkToApplications lm state $ string "allapplications"
              , linkToDerivation lm state $ string "derivation"
              , linkToMicrosteps lm state $ string "microsteps"
              ]
         , munless (noBindings state) $
              h2 "Environment" <> text (environment state)
         , encodePrefix state (statePrefix state)
         ])

-- use allfirsts service of domain reasoner, instead of calling the service
-- directly. Note that the service can be redefined (e.g. for the Ask-Elle tutor)
useAllFirsts :: DomainReasoner -> State a -> Either String [(StepInfo a, State a)]
useAllFirsts dr = unsafePerformIO . useAllFirstsIO dr

useAllFirstsIO :: DomainReasoner -> State a -> IO (Either String [(StepInfo a, State a)])
useAllFirstsIO dr st = do
   srv <- findService dr (newId "allfirsts")
   case serviceFunction srv of
      f ::: tp -> do
         conv <- equalM tp (tState .-> tError (tList (tPair tStepInfo tState)))
         return (conv f st)

encodePrefix :: State a -> Prefix (Context a) -> HTMLBuilder
encodePrefix st =
   mconcat . zipWith3 make [1::Int ..] (stateLabels st) . prefixPaths
 where
   make i ls path = mconcat
      [ h2 $ "Path " ++ show i
      , let count p = text $ length $ filter p prSteps
            enter   = spanClass "step-enter" . text
            comma c = if c == ',' then ", " else [c]
        in keyValueTable
              [ ("path", string $ concatMap comma $ show path)
              , ("steps", count (const True))
              , ("major rules", count isMajor)
              , ("active labels", ul $ map enter ls)
              ]
      , mconcat $ intersperse (string ", ") $ map htmlStep prSteps
      ]
    where
      ex  = exercise st
      ctx = stateContext st
      prSteps = fst $ replayPath path (strategy ex) ctx

htmlStep :: Rule a -> HTMLBuilder
htmlStep r =
   case (isEnterRule r, isExitRule r) of
      (Just l, _) -> spanClass "step-enter" $ string $ "enter " ++ show l
      (_, Just l) -> spanClass "step-exit"  $ string $ "exit " ++ show l
      _ -> let s = if isMinor r then "minor" else "major"
           in spanClass ("step-"++s) $ string $ showId r

htmlDerivationWith :: HTMLBuilder -> (s -> HTMLBuilder) -> (t -> HTMLBuilder) -> Derivation s t -> HTMLBuilder
htmlDerivationWith before forStep forTerm d =
   divClass "derivation" $ mconcat $
      before : forTerm (firstTerm d) :
         [ forStep s <> forTerm a | (_, s, a) <- triples d ]

htmlFirsts :: LinkManager -> HTMLEncoder a [(StepInfo a, State a)]
htmlFirsts lm = encoderFor $ \xs ->
   h2 "Firsts" <>
   ul [ keyValueTable
           [ ("Rule", string $ showId r)
           , ("Location", text loc)
           , ("Term", text $ show $ currentTerm (top $ stateContext s) )
           , ("Focus", text $ show $ currentTerm (stateContext s) )
           , ("Environment", text env)
           ] <> htmlState lm // s
      | ((r, loc, env), s) <- xs
      ]

htmlAllApplications :: LinkManager -> HTMLEncoder a [(Rule (Context a), Location, State a)]
htmlAllApplications lm = encoderFor $ \xs ->
   h2 "All applications" <>
   ul [ keyValueTable
           [ ("Rule", string $ showId r)
           , ("Location", text loc)
           ] <> (if isBuggy r then mempty else htmlState lm // s)
      | (r, loc, s) <- xs
      ]

htmlDiagnosis :: LinkManager -> DomainReasoner -> HTMLEncoder a (Diagnosis a)
htmlDiagnosis lm dr = encoderFor $ \diagnosis ->
   case diagnosis of
      SyntaxError s ->
         spanClass "error" $ string s
      Buggy _ r ->
         spanClass "error" $ string $ "Not equivalent: buggy rule " ++ show r
      NotEquivalent s ->
         spanClass "error" $ string $ if null s then "Not equivalent" else s
      Similar _ s ->
         h2 "Similar term" <> encodeState lm dr // s
      WrongRule _ s mr ->
         h2 ("WrongRule " ++ maybe "" showId mr)
         <> encodeState lm dr // s
      Expected _ s r ->
         h2 ("Expected (" ++ show r ++ ")")
         <> encodeState lm dr // s
      Detour _ s _ r ->
         h2 ("Detour (" ++ show r ++ ")")
         <> encodeState lm dr // s
      Correct _ s ->
         h2 "Correct" <> encodeState lm dr // s
      Unknown _ s ->
         h2 "Unknown" <> encodeState lm dr // s

htmlDescription :: HasId a => a -> HTMLBuilder
htmlDescription a = munless (null (description a)) $
   para $
      bold (string "Description") <> br
      <> spanClass "description" (string (description a))

submitForm :: HTMLBuilder -> HTMLBuilder
submitForm this = element "form"
   [ "name"     .=. "myform"
   , "onsubmit" .=. "return submitTerm()"
   , "method"   .=. "post"
   , this
   , element "input" ["type" .=. "text", "name" .=. "myterm"]
   , element "input" ["type"  .=. "submit", "value" .=. "Submit"]
   ]

-- stateinfo service
submitStateInfo :: LinkManager -> Exercise a -> HTMLBuilder
submitStateInfo lm ex =
   submitForm (string "other exercise: ")
   <> submitRequest lm request
 where
   request = "<request service='stateinfo' exerciseid='" ++ showId ex
          ++ "' encoding='html'><state><expr>\" + getTerm() + \"</expr></state></request>"

-- diagnose service
submitDiagnose :: LinkManager -> State a -> HTMLBuilder
submitDiagnose lm st = submitForm mempty <> submitRequest lm request
 where
   request = "<request service='diagnose' exerciseid='" ++ showId (exercise st)
          ++ "' encoding='html'>" ++ ststr ++ "<expr>\"  + getTerm() + \"</expr></request>"

   ststr   = case fromBuilder (stateToXML st) of
                Just el -> concatMap f (compactXML el)
                Nothing -> ""

   f '\\' = "\\\\"
   f '"'  = "\\\""
   f c    = [c]

submitRequest :: LinkManager -> String -> HTMLBuilder
submitRequest lm request = submitURL $
   quote (urlForRequest lm) ++ "+encodeURIComponent(" ++ quote request ++ ")"

quote :: String -> String
quote s = '"' : s ++ "\""

-- Inject two JavaScript functions for handling the input form
submitURL :: String -> HTMLBuilder
submitURL url = tag "script" $
   ("type" .=. "text/javascript")
   <> unescaped (
      "function getTerm() {\
      \   var s = document.myform.myterm.value;\
      \   var result = '';\
      \   for (var i=0;i<s.length;i++) {\
      \      if (s[i]=='<') result+='&lt;';\
      \      else if (s[i]=='>') result+='&gt;';\
      \      else if (s[i]=='&') result+='&amp;';\
      \      else if (s[i]=='\"') result+='&quot;';\
      \      else if (s[i]==\"'\") result+='&apos;';\
	    \      else result+=s[i];\
      \   }\
      \   return result;\
      \}\
      \function submitTerm() {\
      \   document.myform.action = " ++ url ++ ";\
      \}")