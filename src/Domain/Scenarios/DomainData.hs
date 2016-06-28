{-# LANGUAGE DeriveGeneric #-}
module Domain.Scenarios.DomainData where

import Data.Binary
import GHC.Generics

import Ideas.Text.JSON

data Type
    = TBoolean
    | TInteger
    | TString
    deriving (Show, Read, Eq, Generic)

data Value
    = VBoolean Bool
    | VInteger Integer
    | VString String
    deriving (Show, Read, Eq, Generic)

instance Binary Value

instance InJSON Value where
    toJSON (VBoolean b) = toJSON b
    toJSON (VInteger i) = toJSON i
    toJSON (VString s) = toJSON s
    fromJSON (String s) = return (VString s)
    fromJSON (Number (I i)) = return (VInteger i)
    fromJSON (Boolean b) = return (VBoolean b)
    fromJSON _ = error "fromJSON: not supported"
