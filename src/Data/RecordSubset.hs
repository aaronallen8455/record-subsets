{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Data.RecordSubset
  ( -- * Core Types
    type SubsetSelector
  , type Subset
  , type FullSet
  , type EmptySet
  , type FieldStatus
  , type FieldOn
  , type FieldOff
  , GetFieldStatus
  , type SubsetField
  , FieldWrapper(..)
  , getField
  , fieldToMaybe
  -- * Aeson
  , objectSubset
  , (.=|)
  , (.:|)
  -- * Lens
  , fieldLens
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

data SubsetSelector label
  = Subset label
  | FullSet
  | EmptySet

type Subset label = 'Subset label
type FullSet  = 'FullSet
type EmptySet = 'EmptySet

data FieldStatus = FieldOn | FieldOff

type FieldOn  = 'FieldOn
type FieldOff = 'FieldOff

type family GetFieldStatus (s :: SubsetSelector l) (p :: [l]) :: FieldStatus where
  GetFieldStatus 'FullSet any = 'FieldOn
  GetFieldStatus 'EmptySet any = 'FieldOff
  GetFieldStatus ('Subset s) (s ': rest) = 'FieldOn
  GetFieldStatus s (x ': rest) = GetFieldStatus s rest
  GetFieldStatus s '[] = 'FieldOff

data FieldWrapper (s :: FieldStatus) a where
  Wrapped :: a -> FieldWrapper 'FieldOn a
  Nil     :: FieldWrapper 'FieldOff a

type SubsetField a t ts = FieldWrapper (GetFieldStatus t ts) a

getField :: FieldWrapper FieldOn a -> a
getField (Wrapped a) = a

fieldToMaybe :: FieldWrapper s a -> Maybe a
fieldToMaybe (Wrapped a) = Just a
fieldToMaybe Nil = Nothing

--------------------------------------------------------------------------------
-- Typeclass Instances
--------------------------------------------------------------------------------

instance Functor (FieldWrapper b) where
  fmap f (Wrapped a) = Wrapped $ f a
  fmap _ Nil = Nil

instance Applicative (FieldWrapper 'FieldOff) where
  pure _ = Nil
  _ <*> _ = Nil

instance Applicative (FieldWrapper 'FieldOn) where
  pure a = Wrapped a
  Wrapped f <*> Wrapped a = Wrapped $ f a

instance Monad (FieldWrapper 'FieldOn) where
  Wrapped a >>= f = f a

instance Monad (FieldWrapper 'FieldOff) where
  _ >>= _ = Nil

instance Foldable (FieldWrapper s) where
  foldMap f (Wrapped a) = f a
  foldMap _ Nil = mempty

instance Traversable (FieldWrapper s) where
  traverse f (Wrapped a) = Wrapped <$> f a
  traverse _ Nil = pure Nil

instance Semigroup a => Semigroup (FieldWrapper s a) where
  Wrapped a <> Wrapped b = Wrapped $ a <> b
  Nil <> Nil = Nil

instance Monoid a => Monoid (FieldWrapper 'FieldOn a) where
  mempty = Wrapped mempty

instance Semigroup a => Monoid (FieldWrapper 'FieldOff a) where
  mempty = Nil

instance Show a => Show (FieldWrapper b a) where
  show (Wrapped a) = show a
  show Nil = "(excluded)"

instance Eq a => Eq (FieldWrapper b a) where
  Wrapped a == Wrapped b = a == b
  Nil == Nil = True

instance Ord a => Ord (FieldWrapper s a) where
  compare (Wrapped a) (Wrapped b) = compare a b
  compare Nil Nil = EQ

--------------------------------------------------------------------------------
-- Aeson
--------------------------------------------------------------------------------

-- this instance is okay to use with .:? but shouldn't be relied on for .:
-- use .:| instead.
instance (Applicative (FieldWrapper s), Aeson.FromJSON a)
    => Aeson.FromJSON (FieldWrapper s a) where
  parseJSON = fmap pure . Aeson.parseJSON

newtype SubsetFieldPair =
  SubsetFieldPair { getSubsetFieldPair :: Maybe Aeson.Pair }

instance Aeson.KeyValue SubsetFieldPair where
  a .= b = SubsetFieldPair . Just $ a Aeson..= b

-- | Use in place of 'object' for defining encoding of a record with subsets.
objectSubset :: [SubsetFieldPair] -> Aeson.Value
objectSubset = Aeson.object . mapMaybe getSubsetFieldPair

-- | Use this in place of '.=' for `SubsetField`s.
(.=|) :: Aeson.ToJSON v => T.Text -> FieldWrapper s v -> SubsetFieldPair
_   .=| Nil = SubsetFieldPair Nothing
key .=| Wrapped v = SubsetFieldPair . Just $ key Aeson..= v
infixr 8 .=|

-- | Use this in place of '.:' for `SubsetField`s.
(.:|) :: (Applicative (FieldWrapper s), Aeson.FromJSON v)
      => Aeson.Object -> T.Text -> Aeson.Parser (FieldWrapper s v)
o .:| k = sequenceA $ pure (o Aeson..: k)

--------------------------------------------------------------------------------
-- Lens
--------------------------------------------------------------------------------

-- | A lens that focuses the value inside an active field wrapper.
fieldLens :: Functor f
          => (a -> f b)
          -> FieldWrapper 'FieldOn a
          -> f (FieldWrapper 'FieldOn b)
fieldLens f (Wrapped a) = Wrapped <$> f a

