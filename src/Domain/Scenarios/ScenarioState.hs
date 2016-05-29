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

import Domain.Scenarios.Globals

-- | ScenarioState
-- The state is affected by every step (rule / statement) that has an effect in a strategy
data ScenarioState = ScenarioState ParameterMap (Maybe StatementInfo) Bool
    deriving (Show, Eq, Typeable, Read, Generic)

instance Binary ScenarioState

type ParameterMap = M.Map ID ParameterValue

-- | The effect of a statement on the current state
data Effect = Effect
        { effectIdref      :: ID
        , effectChangeType :: ChangeType
        , effectValue      :: ParameterValue
        }
 deriving (Show, Read, Generic)

instance Binary Effect

-- | The type of change to be made to a parameter
data ChangeType = Set   -- ^ Set the parameter to the given value
                | Delta -- ^ Add a given value to the existing value
    deriving (Show, Eq, Read, Generic)

instance Binary ChangeType

applyEffects :: ScenarioState -> [Effect] -> StatementInfo -> Bool -> ScenarioState
applyEffects (ScenarioState paramMap _ _) paramEffects statInfo end =
    ScenarioState (foldr applyEffect paramMap paramEffects) (Just statInfo) end

-- | Applies the chosen effect to the state
applyEffect :: Effect -> M.Map String ParameterValue -> M.Map String ParameterValue
applyEffect effect stateMap = case effectChangeType effect of
        Set   -> M.insert idref value stateMap
        Delta -> M.insert idref (M.findWithDefault 0 idref stateMap + value) stateMap
    where idref = effectIdref effect
          value = effectValue effect


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
