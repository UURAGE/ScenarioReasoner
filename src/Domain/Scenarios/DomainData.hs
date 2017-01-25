{-# LANGUAGE DeriveGeneric #-}
module Domain.Scenarios.DomainData where

import Control.Monad
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
    | TAttributeRecord (Maybe (String, Type)) [(String, Type)]
    deriving (Show, Read, Eq, Generic)

instance Binary Type

instance InJSON Type where
    toJSON (TSimple simpleType) = toJSON simpleType
    toJSON (TList itemType) = Object [("type", String "list"), ("itemType", toJSON itemType)]
    toJSON (TAttributeRecord contentInfo attributeTypes) = Object $
        [ ("type", String "attributeRecord")
        , ("attributeTypes", Object (map ktpToJSON attributeTypes))
        ] ++ ciToJSON contentInfo
      where ktpToJSON (subName, subType) = (subName, toJSON subType)
            ciToJSON (Just (contentName, contentType)) =
                [ ("contentName", String contentName)
                , ("contentType", toJSON contentType)
                ]
            ciToJSON Nothing = []
    fromJSON val@(String _) = TSimple <$> fromJSON val
    fromJSON val@(Object _) = do
        typeName <- lookupM "type" val
        case typeName of
            String "list" -> lookupM "itemType" val >>= fromJSON
            String "attributeRecord" -> do
                Object attributeVal <- lookupM "attributeTypes" val
                let contentInfo = do
                        String contentName <- lookupM "contentName" val
                        contentType <- lookupM "contentType" val >>= fromJSON
                        return (contentName, contentType)
                TAttributeRecord contentInfo <$> mapM ktpFromJSON attributeVal
                  where ktpFromJSON (subName, subType) = liftM2 (,) (return subName) (fromJSON subType)
            _ -> error "fromJSON: not supported"
    fromJSON _ = error "fromJSON: not supported"

data Value
    = VBoolean Bool
    | VInteger Integer
    | VString String
    | VList [Value]
    | VAttributeRecord [(String, Value)]
    deriving (Show, Read, Eq, Generic)

instance Binary Value

instance InJSON Value where
    toJSON (VBoolean b) = toJSON b
    toJSON (VInteger i) = toJSON i
    toJSON (VString s) = toJSON s
    toJSON (VList xs) = Array (map toJSON xs)
    toJSON (VAttributeRecord xs) = Object (map kvpToJSON xs)
      where kvpToJSON (key, value) = (key, toJSON value)
    fromJSON (String s) = return (VString s)
    fromJSON (Number (I i)) = return (VInteger i)
    fromJSON (Boolean b) = return (VBoolean b)
    fromJSON (Array xs) = VList <$> mapM fromJSON xs
    fromJSON (Object xs) = VAttributeRecord <$> mapM kvpFromJSON xs
      where kvpFromJSON (key, jvalue) = liftM2 (,) (return key) (fromJSON jvalue)
    fromJSON _ = error "fromJSON: not supported"
