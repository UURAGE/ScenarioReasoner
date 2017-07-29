{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, DeriveGeneric #-}
{- © Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.ScenarioState where

import Control.Monad

import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import Data.Binary

import Ideas.Text.JSON
import qualified Ideas.Text.UTF8 as UTF8

import GHC.Generics

import qualified Domain.Scenarios.DomainData as DD
import Domain.Scenarios.Globals

-- | ScenarioState
-- The state is affected by every step (rule / statement) that has an effect in a strategy
data ScenarioState = ScenarioState ParameterState (Maybe StatementInfo)
    deriving (Show, Typeable, Read, Generic)

instance Binary ScenarioState

-- | The effect of a statement on the current state
data Effect = Effect
        { effectIdref        :: ID
        , effectAssignmentOp :: AssignmentOperator
        , effectValue        :: DD.Value
        }
 deriving (Show, Read, Generic)

instance Binary Effect

-- | The type of change to be made to a parameter
data AssignmentOperator = Assign
                        | AddAssign
                        | SubtractAssign
    deriving (Show, Eq, Read, Generic)

instance Binary AssignmentOperator

applyEffects :: TypeMap -> ScenarioState -> Usered (Charactered [Effect]) -> StatementInfo -> ScenarioState
applyEffects tm (ScenarioState paramState _ ) paramEffects statInfo =
    ScenarioState (applyEffectsU tm paramState paramEffects) (Just statInfo)

applyEffectsU :: TypeMap -> Usered (Charactered ParameterMap) -> Usered (Charactered [Effect]) ->
    Usered (Charactered ParameterMap)
applyEffectsU tm (Usered udpm fpm) (Usered ude fe) = Usered
    { useredUserDefined = applyEffectsC tm udpm ude
    , useredFixed = applyEffectsC tm fpm fe
    }

applyEffectsC :: TypeMap -> Charactered ParameterMap -> Charactered [Effect] -> Charactered ParameterMap
applyEffectsC tm (Charactered ipm pcpm) (Charactered ie pce) = Charactered
    { characteredIndependent = foldr (applyEffect tm) ipm ie
    , characteredPerCharacter = M.foldrWithKey processCharEffects pcpm pce
    }
  where processCharEffects characteridref ce =
            M.adjust (\cpm -> foldr (applyEffect tm) cpm ce) characteridref

-- | Applies the given effect to the state
applyEffect :: TypeMap -> Effect -> M.Map String DD.Value -> M.Map String DD.Value
applyEffect tm effect = M.adjust adjuster (effectIdref effect)
    where adjuster = postProcessor . case effectAssignmentOp effect of
            Assign         -> const (effectValue effect)
            AddAssign      -> intAdjuster (+)
            SubtractAssign -> intAdjuster (-)
          intAdjuster op = onDDInteger (`op` fromDDInteger (effectValue effect))
          postProcessor = case M.lookup (effectIdref effect) tm of
            Just (DD.TSimple (DD.TInteger mmin mmax)) -> onDDInteger (DD.clamp mmin mmax)
            _ -> id

fromDDInteger :: DD.Value -> Integer
fromDDInteger (DD.VInteger i) = i
fromDDInteger v = error ("fromDDInteger: Not integral: " ++ show v)

onDDInteger :: (Integer -> Integer) -> DD.Value -> DD.Value
onDDInteger f (DD.VInteger i) = DD.VInteger (f i)
onDDInteger _ v = error ("onDDInteger: Not integral: " ++ show v)

-- ScenarioState to JSON for sending and receiving datatypes in JSON ---------------------------

instance InJSON ScenarioState where
    toJSON (ScenarioState params stat) = Object [parametersToJSON, statInfoToJSON]
        where
            parametersToJSON  =  ("parameters", toJSON params)
            statInfoToJSON = ("statement", toJSON stat)
    fromJSON val@(Object _) =
        do params <- lookupM "parameters" val >>= fromJSON
           return (ScenarioState params Nothing)
    fromJSON _ = fail "fromJSON: expecting an object"

instance InJSON a => InJSON (Usered a) where
   toJSON (Usered udv fv) = Object
        [ ("userDefined", toJSON udv)
        , ("fixed", toJSON fv)
        ]
   fromJSON val =
        do userDefined <- lookupM "userDefined" val >>= fromJSON
           fixed <- lookupM "fixed" val >>= fromJSON
           return (Usered userDefined fixed)

instance InJSON a => InJSON (M.Map String a)  where
    toJSON = Object . map kvpToJSON . M.assocs
        where kvpToJSON (key, value) = (key, toJSON value)
    fromJSON (Object kjvps) = M.fromList <$> mapM kvpFromJSON kjvps
        where kvpFromJSON (key, jvalue) = liftM2 (,) (return key) (fromJSON jvalue)
    fromJSON _ = fail "fromJSON: expecting an object"

instance InJSON StatementInfo  where
    toJSON statInfo = Object (catMaybes [typeToJSON, textToJSON, characterToJSON, pvsToJSON])
      where
        typeToJSON      = Just   ("type",           toJSON        (statType           statInfo))
        textToJSON      = Just   ("text",           toJSON        (statText           statInfo))
        characterToJSON = (\x -> ("character",      toJSON x)) <$> statCharacterIdref statInfo
        pvsToJSON       = Just   ("propertyValues", toJSON        (statPropertyValues statInfo))
    fromJSON _ = fail "fromJSON: not supported"

instance InJSON a => InJSON (Charactered a) where
    toJSON charVal = Object
        [ ("independent",  toJSON (characteredIndependent  charVal))
        , ("perCharacter", toJSON (characteredPerCharacter charVal))
        ]
    fromJSON val =
        do independent <- lookupM "independent" val >>= fromJSON
           perCharacter <- lookupM "perCharacter" val >>= fromJSON
           return (Charactered independent perCharacter)

instance InJSON a => InJSON (Assocs a) where
    toJSON (Assocs kvps) = Object (map kvpToJSON kvps)
        where kvpToJSON (key, value) = (key, toJSON value)
    fromJSON (Object kjvps) = Assocs <$> mapM kvpFromJSON kjvps
        where kvpFromJSON (key, jvalue) = liftM2 (,) (return key) (fromJSON jvalue)
    fromJSON _ = fail "fromJSON: expecting an object"

instance InJSON a => InJSON (Maybe a) where
    toJSON Nothing = Null
    toJSON (Just val) = toJSON val
    fromJSON Null = return Nothing
    fromJSON val = fromJSON val

showJSON :: ScenarioState -> String
showJSON = UTF8.decode . compactJSON . toJSON

readJSON :: String -> Either String ScenarioState
readJSON = either Left (maybe (Left "failed to interpret JSON state") Right . fromJSON) . parseJSON . UTF8.encode
