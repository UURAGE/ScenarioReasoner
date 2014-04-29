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
type ParameterValue = Int

-- | A parameter
data Parameter = Parameter
        { parameterId           :: String
        , parameterName         :: String
        , parameterEmotion      :: Maybe Emotion
        , parameterInitialValue :: Maybe ParameterValue
        , parameterScored       :: Bool
        } deriving (Show, Eq)
type Emotion = String

-- | Returns the initial value of a parameter, or zero if it does not have one.
parameterInitialValueOrZero :: Parameter -> ParameterValue
parameterInitialValueOrZero = fromMaybe 0 . parameterInitialValue

-- | A value describing the type of a statement element 
data StatementElementType = ComputerStatement | PlayerStatement | Conversation
    deriving (Show, Eq, Read)

-- | An effect of a statement on the current state
data Effect = Effect
        { effectIdref      :: String
        , effectChangeType :: ChangeType
        , effectValue      :: ParameterValue
        } deriving (Show, Eq)
data ChangeType = Set | Delta deriving (Show, Eq, Read)

-- | A value describing the type of a piece of text in a conversation
data ConversationTextType = PlayerText
                          | ComputerText
                          | SituationText
    deriving (Show, Eq, Read)

-- | A function to calculate the score based on the current state
data ScoringFunction = Constant Int
                     | Sum [ScoringFunction]
                     | Scale Int ScoringFunction
                     | ParamRef String
                     | IntegeredCondition Condition

-- | Holds all the trees contained in an interleave
data Interleave = Interleave [Tree] [Sequence] --current model does not allow sequences within interleave. But might be necessary later
        
data Sequence = Sequence [Tree] [Interleave] --current model has one top level sequencce that contains only interleaves
        
data Tree = Tree
        { treeID    :: String
        , startID   :: String
        }

instance Show Tree where
    show (Tree a b) = "tree: " ++ show a ++ " start: " ++ show b

-- | Applies an effect to a state.
applyEffect :: Effect -> State -> State
applyEffect effect state = case effectChangeType effect of
        Set -> setParam idref value state
        Delta -> setParam idref ((getParamOrZero idref state) + value) state
    where idref = effectIdref effect
          value = effectValue effect

-- | Calculates the value of a condition based on the given state.
calculateCondition :: Condition -> State -> Bool
calculateCondition mainCondition state = calculate mainCondition
    where calculate :: Condition -> Bool
          calculate condition = case condition of
            And subConditions    -> and . map calculate $ subConditions
            Or subConditions     -> or . map calculate $ subConditions
            Condition comparison -> calculateComparisonCondition comparison state

-- | Calculates the value of a comparison based on the given state.
calculateComparisonCondition :: ComparisonCondition -> State -> Bool
calculateComparisonCondition comparison state = operator tested value
    where operator = calculateCompareOperator (conditionTest comparison)
          tested = getParamOrZero (conditionIdref comparison) state
          value  = conditionValue comparison

-- | Calculates the value of a possible condition based on the given state.
calculateMaybeCondition :: Maybe Condition -> State -> Bool
calculateMaybeCondition = maybe (const True) calculateCondition

-- | Returns the binary predicate corresponding to the given operator type.
calculateCompareOperator :: CompareOperator -> (Int -> Int -> Bool)
calculateCompareOperator operator = case operator of
            LessThan -> (<)
            LessThanEqualTo -> (<=)
            EqualTo -> (==)
            GreaterThanEqualTo -> (>=)
            GreaterThan -> (>)
            NotEqualTo -> (/=)

-- | Calculates the value of a scoring function based on the given state.
calculateScore :: ScoringFunction -> State -> Int
calculateScore mainScoringFunction state = calculate mainScoringFunction  
    where calculate scoringFunction = case scoringFunction of
            Constant value               -> value
            Sum subFunctions             -> sum . map calculate $ subFunctions
            Scale scalar subFunction     -> scalar * calculate subFunction
            ParamRef paramId             -> getParamOrZero paramId state
            IntegeredCondition condition -> if calculateCondition condition state then 1 else 0

-- | Calculates the values of the scored parameters in the given state.
calculateSubScores :: [Parameter] -> State -> [(String, String, Int)]
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

-----------------------------------------------------------------------------
-- | State
-- The state is affected by every step in a strategy.

type State = M.Map String Int

-- State to JSON

instance InJSON a => InJSON (M.Map String a)  where
    toJSON = Object . map kvpToJSON . M.assocs
        where kvpToJSON (key, value) = (key, toJSON value)
    fromJSON (Object kjvps) = liftM M.fromList (mapM kvpFromJSON kjvps)
        where kvpFromJSON (key, jvalue) = liftM2 (,) (return key) (fromJSON jvalue) 
    fromJSON _ = fail "expecting an object"

showJSON :: State -> String
showJSON = compactJSON . toJSON

readJSON :: String -> Either String State
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

getParamOrZero :: String -> State -> Int
getParamOrZero = M.findWithDefault 0

setZero, setOne :: String -> State -> State
setZero = flip M.insert 1
setOne = flip M.insert 1

setParam :: String -> Int -> State -> State
setParam = M.insert

onlyOne :: String -> State -> State
onlyOne s = (flip M.insert 1 s).(M.map (\_-> 0))

isZero, isOne :: String -> State -> Bool
isZero s cf = isVal 0 True s (cf)
isOne s cf = isVal 1 False s (cf)

isEmpty :: State -> Bool
isEmpty cf = M.null (cf)

eitherIsOne :: [String] -> State -> Bool
eitherIsOne [] _      = False
eitherIsOne (x:xs) ck = (isOne x ck) || (eitherIsOne xs ck)

isVal :: Int -> Bool -> String -> State -> Bool
isVal v d s m = case M.lookup s m of
 Nothing -> d
 Just x -> (x == v)
 
andF :: (a->Bool) -> (a->Bool) -> a -> Bool
andF f g a = (f a) && (g a)

orF :: (a->Bool) -> (a->Bool) -> a -> Bool
orF f g a = (f a) || (g a)

emptyState :: State
emptyState = M.empty

fromList :: [(String, Int)] -> State
fromList = M.fromList
