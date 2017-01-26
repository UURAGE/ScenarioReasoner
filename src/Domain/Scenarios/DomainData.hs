{-# LANGUAGE DeriveGeneric, PatternGuards #-}
module Domain.Scenarios.DomainData where

import Control.Arrow
import Control.Monad
import Data.Binary
import Data.Maybe
import GHC.Generics

import Ideas.Text.JSON

data SimpleType
    = TBoolean
    | TInteger (Maybe Integer) (Maybe Integer)
    | TString
    deriving (Show, Read, Eq, Generic)

instance Binary SimpleType

instance InJSON SimpleType where
    toJSON TBoolean = String "boolean"
    toJSON (TInteger Nothing Nothing) = String "integer"
    toJSON (TInteger mmin mmax) = Object $ ("name", String "integer") : catMaybes
        [ (\imin -> ("minimum", toJSON imin)) <$> mmin
        , (\imax -> ("maximum", toJSON imax)) <$> mmax
        ]
    toJSON TString = String "string"
    fromJSON (String "boolean") = return TBoolean
    fromJSON (String "integer") = return (TInteger Nothing Nothing)
    fromJSON (String "string") = return TString
    fromJSON val@(Object _) | Just (String typeName) <- lookupM "name" val = case typeName of
        "integer" -> do
            mmin <- fromJSON <$> lookupM "minimum" val
            mmax <- fromJSON <$> lookupM "maximum" val
            return (TInteger mmin mmax)
        _ -> error "fromJSON: not supported"
    fromJSON _ = error "fromJSON: not supported"

unrestrictSimpleType :: SimpleType -> SimpleType
unrestrictSimpleType TBoolean = TBoolean
unrestrictSimpleType (TInteger _ _) = TInteger Nothing Nothing
unrestrictSimpleType TString = TString

data Type
    = TSimple SimpleType
    | TList Type
    | TAttributeRecord (Maybe (String, Type)) [(String, Type)]
    deriving (Show, Read, Eq, Generic)

instance Binary Type

instance InJSON Type where
    toJSON (TSimple simpleType) = toJSON simpleType
    toJSON (TList itemType) = Object [("name", String "list"), ("itemType", toJSON itemType)]
    toJSON (TAttributeRecord contentInfo attributeTypes) = Object $
        [ ("name", String "attributeRecord")
        , ("attributeTypes", Object (map ktpToJSON attributeTypes))
        ] ++ ciToJSON contentInfo
      where ktpToJSON (subName, subType) = (subName, toJSON subType)
            ciToJSON (Just (contentName, contentType)) =
                [ ("contentName", String contentName)
                , ("contentType", toJSON contentType)
                ]
            ciToJSON Nothing = []
    fromJSON val@(String _) = TSimple <$> fromJSON val
    fromJSON val@(Object _) | Just (String typeName) <- lookupM "name" val = case typeName of
        "list" -> TList <$> (lookupM "itemType" val >>= fromJSON)
        "attributeRecord" -> do
            Object attributeVal <- lookupM "attributeTypes" val
            let contentInfo = do
                    String contentName <- lookupM "contentName" val
                    contentType <- lookupM "contentType" val >>= fromJSON
                    return (contentName, contentType)
            TAttributeRecord contentInfo <$> mapM ktpFromJSON attributeVal
              where ktpFromJSON (subName, subType) = liftM2 (,) (return subName) (fromJSON subType)
        _ -> TSimple <$> fromJSON val
    fromJSON _ = error "fromJSON: not supported"

unrestrictType :: Type -> Type
unrestrictType (TSimple simpleType) = TSimple (unrestrictSimpleType simpleType)
unrestrictType (TList itemType) = TList (unrestrictType itemType)
unrestrictType (TAttributeRecord contentInfo attributeTypes) = TAttributeRecord
    (second unrestrictType <$> contentInfo)
    (map (second unrestrictType) attributeTypes)

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

clamp :: Maybe Integer -> Maybe Integer -> Integer -> Integer
clamp mmin mmax i
    | Just imax <- mmax, i > imax = imax
    | Just imin <- mmin, i < imin = imin
    | otherwise = i
