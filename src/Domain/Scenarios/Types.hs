{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-} 
module Domain.Scenarios.Types where

import Control.Monad
import Data.Char
import qualified Data.Map as M
import Data.Maybe

import Ideas.Common.Library hiding (Sum)
import Ideas.Common.Utils
import Ideas.Text.JSON

-----------------------------------------------------------------------------

type ParameterValue = Int
type Emotion = String


-- | A condition
data Condition = And [Condition] -- ^ A list of conditions, all of which need to be satisfied 
               | Or [Condition] -- ^ A list of conditions, one of which needs to be satisfied
               | Condition ComparisonCondition -- ^ A comparison condition
               deriving (Show, Eq)

-- | A condition that compares the value of a parameter using a binary predicate
data ComparisonCondition = ComparisonsCondition
        { conditionIdref :: String
        , conditionTest  :: CompareOperator
        , conditionValue :: ParameterValue
        } deriving (Show, Eq)
data CompareOperator = LessThan
                     | LessThanEqualTo
                     | EqualTo
                     | GreaterThanEqualTo
                     | GreaterThan
                     | NotEqualTo
                     deriving (Show, Eq, Read)
                     
-- | Datastructure for a function to calculate the score based on the current state
data ScoringFunction = Constant Int
                     | Sum [ScoringFunction]
                     | Scale Int ScoringFunction
                     | ParamRef String
                     | IntegeredCondition Condition    

data Parameter = Parameter
        { parameterId           :: String
        , parameterName         :: String
        , parameterEmotion      :: Maybe Emotion
        , parameterInitialValue :: Maybe ParameterValue
        , parameterScored       :: Bool
        } deriving (Show, Eq)

-- | Returns the initial value of a parameter, or zero if it does not have one.
parameterInitialValueOrZero :: Parameter -> ParameterValue
parameterInitialValueOrZero = fromMaybe 0 . parameterInitialValue

-- | A value describing the type of a statement element 
data StatementElementType = ComputerStatement | PlayerStatement | Conversation
    deriving (Show, Eq, Read)

-- | A value describing the type of a piece of text in a conversation
data ConversationTextType = PlayerText
                          | ComputerText
                          | SituationText
    deriving (Show, Eq, Read)


-- | Holds all the trees contained in an interleave
data Interleave = Interleave [Tree] [Sequence] --current model does not allow sequences within interleave. But might be necessary later
        
data Sequence = Sequence [Tree] [Interleave] --current model has one top level sequencce that contains only interleaves
        
data Tree = Tree
        { treeID        :: String
        , treeStartID   :: String
        , treeAtomic    :: String
        }
        

instance Show Tree where
    show (Tree a b c) = "tree: " ++ show a ++ " start: " ++ show b ++ " atomic: " ++ show c

-- | Calculates the value of a condition based on the given state.
evaluateCondition :: Condition -> EmotionalState -> Bool
evaluateCondition mainCondition state = evaluate mainCondition
    where evaluate :: Condition -> Bool
          evaluate condition = case condition of
            And subConditions    -> and . map evaluate $ subConditions
            Or  subConditions    -> or  . map evaluate $ subConditions
            Condition comparison -> evaluateComparisonCondition comparison state

-- | Calculates the value of a comparison based on the given state.
evaluateComparisonCondition :: ComparisonCondition -> EmotionalState -> Bool
evaluateComparisonCondition comparison state = operator tested value
    where operator = getCompareOperator (conditionTest comparison)
          tested = getParamOrZero (conditionIdref comparison) state
          value  = conditionValue comparison

-- | Calculates the value of a possible condition based on the given state.
evaluateMaybeCondition :: Maybe Condition -> EmotionalState -> Bool
evaluateMaybeCondition = maybe (const True) evaluateCondition

-- | Returns the binary predicate corresponding to the given operator type.
getCompareOperator :: CompareOperator -> (Int -> Int -> Bool)
getCompareOperator operator = case operator of
            LessThan           -> (<)
            LessThanEqualTo    -> (<=)
            EqualTo            -> (==)
            GreaterThanEqualTo -> (>=)
            GreaterThan        -> (>)
            NotEqualTo         -> (/=)
                     
-- | Calculates the value of a scoring function based on the given state.
calculateScore :: ScoringFunction -> EmotionalState -> Int
calculateScore mainScoringFunction state = calculate mainScoringFunction  
    where calculate scoringFunction = case scoringFunction of
            Constant value               -> value
            Sum subFunctions             -> sum . map calculate $ subFunctions
            Scale scalar subFunction     -> scalar * calculate subFunction
            ParamRef paramId             -> getParamOrZero paramId state
            IntegeredCondition condition -> if evaluateCondition condition state then 1 else 0

-- | Calculates the values of the scored parameters in the given state.
calculateSubScores :: [Parameter] -> EmotionalState -> [(String, String, Int)]
calculateSubScores parameters state = 
    map (\param -> ( parameterId param
                   , parameterName param
                   , getParamOrZero (parameterId param) state)
        ) . filter parameterScored $ parameters


-- | Returns the value to be used to represent a statement type in a rule ID.
toIdTypeSegment :: StatementElementType -> String
toIdTypeSegment = takeWhile isLower . applyToFirst toLower . show

-- | Applies a function to the first element of a list, if there is one.
applyToFirst :: (a -> a) -> [a] -> [a]
applyToFirst f (x:xs) = (f x) : xs
applyToFirst _ [] = []

errorOnFail :: Maybe a -> a
errorOnFail = fromMaybe (error "failed...")

-----------------------------------------------------------------------------
-- | EmotionalState
-- The state is affected by every step in a strategy.

type EmotionalState = (M.Map String Int, String)

-- | The effect of a statement on the current emotional state
data Effect = Effect
        { effectIdref      :: String
        , effectChangeType :: ChangeType
        , effectValue      :: ParameterValue
        } deriving (Show, Eq)
data ChangeType = Set | Delta deriving (Show, Eq, Read)

-- | Applies the chosen effect to the emotional state
applyEffect :: Effect -> EmotionalState -> EmotionalState
applyEffect effect state = case effectChangeType effect of
        Set   -> setParam idref value state
        Delta -> setParam idref ((getParamOrZero idref state) + value) state
    where idref = effectIdref effect
          value = effectValue effect

-- EmotionalState to JSON

instance InJSON a => InJSON (M.Map String a)  where
    toJSON = Object . map kvpToJSON . M.assocs
        where kvpToJSON (key, value) = (key, toJSON value)
    fromJSON (Object kjvps) = liftM M.fromList (mapM kvpFromJSON kjvps)
        where kvpFromJSON (key, jvalue) = liftM2 (,) (return key) (fromJSON jvalue) 
    fromJSON _ = fail "expecting an object"

showJSON :: EmotionalState -> String
showJSON = compactJSON . toJSON

readJSON :: String -> Either String EmotionalState
readJSON = either Left (maybe (Left "failed to interpret JSON state") Right . fromJSON) . parseJSON

-- - -
-- Instances of isTerm
--   toTerm   :: a -> Term
--   fromTerm :: MonadPlus m => Term -> m a
-- - -

instance IsTerm (M.Map String Int) where
 toTerm = toTerm . M.toAscList . (M.mapKeysMonotonic ShowString)
 fromTerm x = do 
   x' <- fromTerm x
   return (M.mapKeysMonotonic fromShowString (M.fromDistinctAscList x'))

getParamOrZero :: String -> EmotionalState -> Int
getParamOrZero name state = M.findWithDefault 0 name (fst state)

setZero, setOne :: String -> EmotionalState -> EmotionalState
setZero name state = (flip M.insert 1 name (fst state), snd state)
setOne name state = (flip M.insert 1 name (fst state), snd state)

setParam :: String -> Int -> EmotionalState -> EmotionalState
setParam name value state = (M.insert name value (fst state), snd state)

onlyOne :: String -> EmotionalState -> EmotionalState
onlyOne name state = ((flip M.insert 1 name).(M.map (\_-> 0)) $ (fst state), snd state)

isZero, isOne :: String -> EmotionalState -> Bool
isZero s cf = isVal 0 True s (cf)
isOne s cf = isVal 1 False s (cf)

isEmpty :: EmotionalState -> Bool
isEmpty cf = M.null (fst cf)

eitherIsOne :: [String] -> EmotionalState -> Bool
eitherIsOne [] _      = False
eitherIsOne (x:xs) ck = (isOne x ck) || (eitherIsOne xs ck)

isVal :: Int -> Bool -> String -> EmotionalState -> Bool
isVal v d s m = case M.lookup s (fst m) of
 Nothing -> d
 Just x -> (x == v)
 
andF :: (a->Bool) -> (a->Bool) -> a -> Bool
andF f g a = (f a) && (g a)

orF :: (a->Bool) -> (a->Bool) -> a -> Bool
orF f g a = (f a) || (g a)

--emptyState :: EmotionalState
--emptyState = (M.empty, "")

fromList :: [(String, Int)] -> EmotionalState
fromList list = (M.fromList list, "")

----------------------------------------------------------------------------------------------------------------
