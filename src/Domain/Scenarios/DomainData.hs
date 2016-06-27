{-# LANGUAGE DeriveGeneric #-}
module Domain.Scenarios.DomainData where

import Data.Binary
import GHC.Generics

import Ideas.Text.JSON

data Type
    = TBoolean
    | TInteger
    | TString
    | TEnumeration
    deriving (Show, Read, Eq, Generic)

data Value
    = VBoolean Bool
    | VInteger Integer
    | VString String
    | VEnumeration Value
    deriving (Show, Read, Eq, Generic)

instance Binary Value

instance InJSON Value where
    toJSON (VBoolean b) = toJSON b
    toJSON (VInteger i) = toJSON i
    toJSON (VString s) = toJSON s
    toJSON (VEnumeration v) = Object [("enumeratedValue", toJSON v)]
    fromJSON (String s) = return (VString s)
    fromJSON (Number (I i)) = return (VInteger i)
    fromJSON (Boolean b) = return (VBoolean b)
    fromJSON (Object [("enumeratedValue", v)]) = fmap VEnumeration (fromJSON v)
    fromJSON _ = error "fromJSON: not supported"
