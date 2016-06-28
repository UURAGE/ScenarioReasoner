{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, DeriveGeneric #-}
{- ©Copyright Utrecht University (Department of Information and Computing Sciences) -}

module Domain.Scenarios.ScenarioState where

import Control.Monad

import qualified Data.Map as M
import Data.Typeable
import Data.Binary

import Ideas.Text.JSON
import qualified Ideas.Text.UTF8 as UTF8

import GHC.Generics

import qualified Domain.Scenarios.DomainData as DD
import Domain.Scenarios.Globals

-- | ScenarioState
-- The state is affected by every step (rule / statement) that has an effect in a strategy
data ScenarioState = ScenarioState ParameterState (Maybe StatementInfo) Bool
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

applyEffects :: ScenarioState -> Usered (Charactered [Effect]) -> StatementInfo -> Bool -> ScenarioState
applyEffects (ScenarioState paramState _ _) paramEffects statInfo end =
    ScenarioState (applyEffectsU paramState paramEffects) (Just statInfo) end

applyEffectsU :: Usered (Charactered ParameterMap) -> Usered (Charactered [Effect]) ->
    Usered (Charactered ParameterMap)
applyEffectsU (Usered udpm fpm) (Usered ude fe) = Usered
    { useredUserDefined = applyEffectsC udpm ude
    , useredFixed = applyEffectsC fpm fe
    }

applyEffectsC :: Charactered ParameterMap -> Charactered [Effect] -> Charactered ParameterMap
applyEffectsC (Charactered ipm pcpm) (Charactered ie pce) = Charactered
    { characteredIndependent = foldr applyEffect ipm ie
    , characteredPerCharacter = M.foldrWithKey processCharEffects pcpm pce
    }
  where processCharEffects characteridref ce =
            M.adjust (\cpm -> foldr applyEffect cpm ce) characteridref

-- | Applies the given effect to the state
applyEffect :: Effect -> M.Map String DD.Value -> M.Map String DD.Value
applyEffect effect = M.adjust adjuster (effectIdref effect)
    where adjuster = case effectAssignmentOp effect of
            Assign         -> const (effectValue effect)
            AddAssign      -> intAdjuster (+)
            SubtractAssign -> intAdjuster (-)
          intAdjuster op (DD.VInteger i) = DD.VInteger (i `op` intEffectValue)
          intAdjuster _ v = error ("intAdjuster: Not integral: " ++ show v)
          intEffectValue = case effectValue effect of
            DD.VInteger i -> i
            v             -> error ("intEffectValue: Not integral: " ++ show v)

-- ScenarioState to JSON for sending and receiving datatypes in JSON ---------------------------

instance InJSON ScenarioState where
    toJSON (ScenarioState params stat end) = Object [parametersToJSON, statInfoToJSON, endToJSON]
        where
            parametersToJSON  =  ("parameters", toJSON params)
            statInfoToJSON = ("statement", toJSON stat)
            endToJSON = ("internal", Object[("end", toJSON end)])
    fromJSON val@(Object _) =
        do params <- lookupM "parameters" val >>= fromJSON
           -- The internal object containing end MUST be sent back to the reasoner if it exists
           end <- lookupM "internal" val >>= lookupM "end" >>= fromJSON
           return (ScenarioState params Nothing end)
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
    toJSON statInfo = Object [typeToJSON, textToJSON, pvsToJSON]
      where
        typeToJSON      = ("type",      toJSON (statType        statInfo))
        textToJSON      = ("text",      toJSON (statText        statInfo))
        pvsToJSON       = ("propertyValues", toJSON (statPropertyValues statInfo))
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
