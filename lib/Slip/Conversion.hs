{-# LANGUAGE FlexibleInstances #-}

module Slip.Conversion
  ( FromScheme(..)
  , ToScheme(..)
  , FrozenSchemeValue(..)
  ) where

import Slip.Core

class FromScheme a where
  fromScheme :: SchemeValue s -> Maybe a

class ToScheme a where
  toScheme :: a -> SchemeValue s

instance FromScheme Bool where
  fromScheme (SchemeBoolean b) = Just b
  fromScheme _ = Nothing

instance FromScheme Integer where
  fromScheme (SchemeNumber i) = Just i
  fromScheme _ = Nothing

instance FromScheme String where
  fromScheme (SchemeString s) = Just s
  fromScheme _ = Nothing

instance FromScheme a => FromScheme [a] where
  fromScheme (SchemeList vals) = mapM fromScheme vals
  fromScheme _ = Nothing

instance ToScheme Bool where
  toScheme = SchemeBoolean

instance ToScheme Integer where
  toScheme = SchemeNumber

instance ToScheme String where
  toScheme = SchemeString

data FrozenSchemeValue
  = FrozenSchemeBoolean Bool
  | FrozenSchemeNumber Integer
  | FrozenSchemeString String
  | FrozenSchemeSymbol String
  | FrozenSchemeList [FrozenSchemeValue]
  | FrozenSchemeDottedList [FrozenSchemeValue] FrozenSchemeValue
  deriving (Eq)

instance Show FrozenSchemeValue where
  show (FrozenSchemeBoolean True) = "#t"
  show (FrozenSchemeBoolean False) = "#f"
  show (FrozenSchemeNumber i) = show i
  show (FrozenSchemeString s) = "\"" ++ s ++ "\""
  show (FrozenSchemeSymbol s) = s
  show (FrozenSchemeList l) = "(" ++ unwords (map show l) ++ ")"
  show (FrozenSchemeDottedList l v) = "(" ++ unwords (map show l) ++ " . " ++ show v ++ ")"

instance FromScheme FrozenSchemeValue where
  fromScheme (SchemeBoolean b) = Just $ FrozenSchemeBoolean b
  fromScheme (SchemeNumber i) = Just $ FrozenSchemeNumber i
  fromScheme (SchemeString s) = Just $ FrozenSchemeString s
  fromScheme (SchemeSymbol s) = Just $ FrozenSchemeSymbol s
  fromScheme (SchemeList l) = FrozenSchemeList <$> mapM fromScheme l
  fromScheme (SchemeDottedList l v) = FrozenSchemeDottedList <$> mapM fromScheme l <*> fromScheme v
  fromScheme _ = Nothing

instance ToScheme FrozenSchemeValue where
  toScheme (FrozenSchemeBoolean b) = SchemeBoolean b
  toScheme (FrozenSchemeNumber i) = SchemeNumber i
  toScheme (FrozenSchemeString s) = SchemeString s
  toScheme (FrozenSchemeSymbol s) = SchemeSymbol s
  toScheme (FrozenSchemeList l) = SchemeList (map toScheme l)
  toScheme (FrozenSchemeDottedList l v) = SchemeDottedList (map toScheme l) (toScheme v)
