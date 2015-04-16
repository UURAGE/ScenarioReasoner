{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, FlexibleContexts #-}
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
--  $Id: Types.hs 7524 2015-04-08 07:31:15Z bastiaan $

module Ideas.Service.Types
   ( -- * Services
     Service, makeService, deprecate
   , serviceDeprecated, serviceFunction
     -- * Types
   , TypeRep(..), Const(..), Type, TypedValue(..)
   , Equal(..), ShowF(..), equalM
     -- * Constructing types
   , tEnvironment, tLocation, tRule, tTuple3, tTuple4, tTuple5, tPair
   , tStrategy, tTree, tState, tBool, tMaybe, tString, tList
   , tId, tService, tSomeExercise, tText, tDifficulty, tContext
   , tDerivation, tError, (.->), tIO, tExercise, tTestSuiteResult, tStdGen
   , tScript, tExamples, tStrategyCfg, tInt
   ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Tree
import Ideas.Common.Library
import Ideas.Common.Utils
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.State
import System.Random
import qualified Ideas.Common.Utils.TestSuite as TestSuite

-----------------------------------------------------------------------------
-- Services

data Service = S
   { serviceId         :: Id
   , serviceDeprecated :: Bool
   , serviceFunction   :: forall a . TypedValue (Type a)
   }

instance Show Service where
   show = showId

instance HasId Service where
   getId = serviceId
   changeId f a = a { serviceId = f (serviceId a) }

makeService :: String -> String -> (forall a . TypedValue (Type a)) -> Service
makeService s descr f = describe descr (S (newId s) False f)

deprecate :: Service -> Service
deprecate s = s { serviceDeprecated = True }

class Equal f where
   equal :: f a -> f b -> Maybe (a -> b)

equalM :: Monad m => Type a t1 -> Type a t2 -> m (t1 -> t2)
equalM t1 t2 = maybe (fail msg) return (equal t1 t2)
 where msg = "Types not equal: " ++ show t1 ++ " and " ++ show t2

instance Equal f => Equal (TypeRep f) where
   equal (Iso p a)  t2         = fmap (. to p) (equal a t2)
   equal t1         (Iso p b)  = fmap (from p .) (equal t1 b)
   equal (a :-> b)  (c :-> d)  = liftM2 (\f g h -> g . h . f)
                                        (equal c a) (equal b d)
   equal (Pair a b) (Pair c d) = liftM2 (***) (equal a c) (equal b d)
   equal (a :|: b)  (c :|: d)  = liftM2 biMap (equal a c) (equal b d)
   equal (List a)   (List b)   = fmap map (equal a b)
   equal (Tag s1 a) (Tag s2 b) | s1 == s2 = equal a b
   equal Unit       Unit       = Just id
   equal (Const a)  (Const b)  = equal a b
   equal _          _          = Nothing

instance Equal (Const a) where
   equal Int         Int         = Just id
   equal Bool        Bool        = Just id
   equal String      String      = Just id
   equal Service     Service     = Just id
   equal Exercise    Exercise    = Just id
   equal Strategy    Strategy    = Just id
   equal State       State       = Just id
   equal Rule         Rule       = Just id
   equal Context     Context     = Just id
   equal Id          Id          = Just id
   equal Location    Location    = Just id
   equal Script      Script      = Just id
   equal StratCfg    StratCfg    = Just id
   equal Environment Environment = Just id
   equal SomeExercise SomeExercise = Just id
   equal Text        Text        = Just id
   equal StdGen      StdGen      = Just id
   equal Result      Result      = Just id
   equal _           _           = Nothing

infixr 5 :|:

-----------------------------------------------------------------------------
-- Types

infix  2 :::
infixr 3 :->

data TypedValue f where
   (:::) :: t -> f t -> TypedValue f

type Type a = TypeRep (Const a)

data TypeRep f t where
   -- Type isomorphisms (for defining type synonyms)
   Iso   :: Isomorphism t1 t2 -> TypeRep f t1 -> TypeRep f t2
   -- Function type
   (:->) :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (t1 -> t2)
   -- Input/output
   IO    :: TypeRep f t -> TypeRep f (IO t)
   -- Special annotations
   Tag   :: String -> TypeRep f t1 -> TypeRep f t1
   -- Type constructors
   List  :: TypeRep f t  -> TypeRep f [t]
   Pair  :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (t1, t2)
   (:|:) :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (Either t1 t2)
   Unit  :: TypeRep f ()
   -- Type constants
   Const :: f t -> TypeRep f t

data Const a t where
   -- exercise specific
   Service      :: Const a Service
   Exercise     :: Const a (Exercise a)
   Strategy     :: Const a (Strategy (Context a))
   State        :: Const a (State a)
   Rule         :: Const a (Rule (Context a))
   Context      :: Const a (Context a)
   -- other types
   Id           :: Const a Id
   Location     :: Const a Location
   Script       :: Const a Script
   StratCfg     :: Const a StrategyCfg
   Environment  :: Const a Environment
   Text         :: Const a Text
   StdGen       :: Const a StdGen
   Result       :: Const a TestSuite.Result
   SomeExercise :: Const a (Some Exercise)
   -- basic types
   Bool         :: Const a Bool
   Int          :: Const a Int
   String       :: Const a String

class ShowF f where
   showF :: f a -> String

instance ShowF f => ShowF (TypeRep f) where
   showF = show

instance ShowF f => Show (TypeRep f t) where
   show (Iso _ t)      = show t
   show (t1 :-> t2)    = show t1 ++ " -> " ++ show t2
   show (IO t)         = show t
   show t@(Pair _ _)   = showTuple t
   show (t1 :|: t2)    = show t1 ++ " | " ++ show t2
   show (Tag s _)      = s
   show (List t)       = "[" ++ show t ++ "]"
   show Unit           = "()"
   show (Const c)      = showF c

instance Show (TypedValue f) => Show (TypedValue (TypeRep f)) where
   show (val ::: tp) =
      case tp of
         Iso iso t  -> show (to iso val ::: t)
         _ :-> _    -> "<<function>>"
         IO _       -> "<<io>>"
         Tag _ t    -> show (val ::: t)
         List t     -> showAsList (map (show . (::: t)) val)
         Pair t1 t2 -> "(" ++ show (fst val ::: t1) ++
                       "," ++ show (snd val ::: t2) ++ ")"
         t1 :|: t2  -> either (show . (::: t1)) (show . (::: t2)) val
         Unit       -> "()"
         Const t    -> show (val ::: t)

showAsList :: [String] -> String
showAsList xs = "[" ++ intercalate "," xs ++ "]"

instance Show (TypedValue (Const a)) where
   show (val ::: tp) =
      case tp of
         Service          -> showId val
         Exercise         -> showId val
         Strategy         -> show val
         Rule             -> showId val
         Id               -> showId val
         SomeExercise     -> case val of Some ex -> showId ex
         State            -> show val
         Context          -> show (location val, environment val)
         Location         -> show val
         Script           -> show val
         StratCfg         -> show val
         Environment      -> show val
         Text             -> show val
         StdGen           -> show val
         Result           -> show val
         Bool             -> map toLower (show val)
         Int              -> show val
         String           -> val

instance Show (Const a t) where
   show = showF

instance ShowF (Const a) where
   showF Service      = "Service"
   showF Exercise     = "Exercise"
   showF Strategy     = "Strategy"
   showF State        = "State"
   showF Rule         = "Rule"
   showF Context      = "Context"
   showF Id           = "Id"
   showF Location     = "Location"
   showF Script       = "Script"
   showF StratCfg     = "StrategyConfiguration"
   showF Environment  = "Environment"
   showF Text         = "TextMessage"
   showF StdGen       = "StdGen"
   showF Result       = "TestSuiteResult"
   showF SomeExercise = "Exercise"
   showF Bool         = "Bool"
   showF Int          = "Int"
   showF String       = "String"

showTuple :: ShowF f => TypeRep f t -> String
showTuple tp = "(" ++ intercalate ", " (collect tp) ++ ")"
 where
   collect :: ShowF f => TypeRep f t -> [String]
   collect (Pair t1 t2) = collect t1 ++ collect t2
   collect (Iso _ t)    = collect t
   collect t            = [showF t]

---------------------------------------------------------------

tError :: Type a t -> Type a (Either String t)
tError = (:|:) tString

tDerivation :: Type a t1 -> Type a t2 -> Type a (Derivation t1 t2)
tDerivation t1 t2 = Tag "Derivation" $ Iso (f <-> g) tp
 where
   tp = tPair t2 (tList (tPair t1 t2))
   f (a, xs) = foldl extend (emptyDerivation a) xs
   g d = (firstTerm d, [ (s, a) | (_, s, a) <- triples d ])

tIO :: Type a t -> Type a (IO t)
tIO = IO

tText :: Type a Text
tText = Const Text

infixr 5 .->

(.->) :: Type a t1 -> Type a t2 -> Type a (t1 -> t2)
(.->) = (:->)

tState :: Type a (State a)
tState = Const State

tMaybe :: Type a t -> Type a (Maybe t)
tMaybe t = Iso (f <-> g) (t :|: Unit)
    where
      f = either Just (const Nothing)
      g = maybe (Right ()) Left

tStrategyCfg :: Type a StrategyCfg
tStrategyCfg = Const StratCfg

tList :: Type a t -> Type a [t]
tList = List

tPair :: Type a t1 -> Type a t2 -> Type a (t1, t2)
tPair = Pair

tString :: Type a String
tString = Const String

tExercise :: Type a (Exercise a)
tExercise = Const Exercise

tContext :: Type a (Context a)
tContext = Const Context

tBool :: Type a Bool
tBool = Const Bool

tInt :: Type a Int
tInt = Const Int

tRule :: Type a (Rule (Context a))
tRule = Const Rule

tLocation :: Type a Location
tLocation = Const Location

tTuple3 :: Type a t1 -> Type a t2 -> Type a t3 -> Type a (t1, t2, t3)
tTuple3 t1 t2 t3 = Iso (f <-> g) (Pair t1 (Pair t2 t3))
    where
      f (a, (b, c)) = (a, b, c)
      g (a, b, c)   = (a, (b, c))

tTuple4 :: Type a t1 -> Type a t2 -> Type a t3 -> Type a t4 -> Type a (t1, t2, t3, t4)
tTuple4 t1 t2 t3 t4 = Iso (f <-> g) (Pair t1 (Pair t2 (Pair t3 t4)))
    where
      f (a, (b, (c, d))) = (a, b, c, d)
      g (a, b, c, d)     = (a, (b, (c, d)))

tTuple5 :: Type a t1 -> Type a t2 -> Type a t3 -> Type a t4 -> Type a t5 -> Type a (t1, t2, t3, t4, t5)
tTuple5 t1 t2 t3 t4 t5 = Iso (f <-> g) (Pair t1 (Pair t2 (Pair t3 (Pair t4 t5))))
    where
      f (a, (b, (c, (d, e)))) = (a, b, c, d, e)
      g (a, b, c, d, e)       = (a, (b, (c, (d, e))))

tEnvironment :: Type a Environment
tEnvironment = Const Environment

tDifficulty :: Type a Difficulty
tDifficulty = Tag "Difficulty" (Iso (f <-> show) tString)
    where
      f = fromMaybe Medium . readDifficulty

tStdGen :: Type a StdGen
tStdGen = Const StdGen

tExamples :: Type a (Examples (Context a))
tExamples = tList (tPair tDifficulty tContext)

tId :: Type a Id
tId = Const Id

tScript :: Type a Script
tScript = Const Script

tSomeExercise :: Type a (Some Exercise)
tSomeExercise = Const SomeExercise

tService :: Type a Service
tService = Const Service

tStrategy :: Type a (Strategy (Context a))
tStrategy = Const Strategy

tTree :: Type a t -> Type a (Tree t)
tTree t = Tag "Tree" $ Iso (f <-> g) (tPair t (tList (tTree t)))
    where
      f = uncurry Node
      g (Node a xs) = (a, xs)

tTestSuiteResult :: Type a TestSuite.Result
tTestSuiteResult = Const Result