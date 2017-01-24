{-# LANGUAGE DeriveGeneric #-}
module Domain.Scenarios.DomainData where

import Data.Binary
import GHC.Generics

import Ideas.Text.JSON

data SimpleType
    = TBoolean
    | TInteger
    | TString
    deriving (Show, Read, Eq, Generic)

instance Binary SimpleType

instance InJSON SimpleType where
    toJSON TBoolean = String "boolean"
    toJSON TInteger = String "integer"
    toJSON TString = String "string"
    fromJSON (String "boolean") = return TBoolean
    fromJSON (String "integer") = return TInteger
    fromJSON (String "string") = return TString
    fromJSON _ = error "fromJSON: not supported"

data Type
    = TSimple SimpleType
    | TList Type
    deriving (Show, Read, Eq, Generic)

instance Binary Type

instance InJSON Type where
    toJSON (TSimple simpleType) = toJSON simpleType
    toJSON (TList itemType) = Object [("type", String "list"), ("itemType", toJSON itemType)]
    fromJSON val@(String _) = TSimple <$> fromJSON val
    fromJSON val@(Object _) = do
        typeName <- lookupM "type" val
        case typeName of
            String "list" -> lookupM "itemType" val >>= fromJSON
            _ -> error "fromJSON: not supported"
    fromJSON _ = error "fromJSON: not supported"

data Value
    = VBoolean Bool
    | VInteger Integer
    | VString String
    | VList [Value]
    deriving (Show, Read, Eq, Generic)

instance Binary Value

instance InJSON Value where
    toJSON (VBoolean b) = toJSON b
    toJSON (VInteger i) = toJSON i
    toJSON (VString s) = toJSON s
    toJSON (VList xs) = Array (map toJSON xs)
    fromJSON (String s) = return (VString s)
    fromJSON (Number (I i)) = return (VInteger i)
    fromJSON (Boolean b) = return (VBoolean b)
    fromJSON (Array xs) = VList <$> mapM fromJSON xs
    fromJSON _ = error "fromJSON: not supported"
