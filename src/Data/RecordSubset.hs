{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- for nested type families
module Data.RecordSubset
  ( -- * Core Types
    type SubsetSelector
  , type Subset
  , type FullSet
  , type EmptySet
  , type Union
  , type Intersection
  , type Difference
  , type SymDiff
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
  , (.:|?)
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
  | Union (SubsetSelector label) (SubsetSelector label)
  | Intersection (SubsetSelector label) (SubsetSelector label)
  | Difference (SubsetSelector label) (SubsetSelector label)
  | SymDiff (SubsetSelector label) (SubsetSelector label)

-- | Selects the subset of fields for the given label
type Subset label     = 'Subset label
-- | Selects the full set of fields
type FullSet          = 'FullSet
-- | Selects the empty set
type EmptySet         = 'EmptySet
-- | Selects the union of two subsets
type Union a b        = 'Union a b
-- | Selects the intersection of two subsets
type Intersection a b = 'Intersection a b
-- | Removes members of the second subset from the first
type Difference a b   = 'Difference a b
-- | The symmetric difference of two subsets
type SymDiff a b      = 'SymDiff a b

data FieldStatus = FieldOn | FieldOff

type FieldOn  = 'FieldOn
type FieldOff = 'FieldOff

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
-- Type Functions
--------------------------------------------------------------------------------

type family Conjunction (a :: FieldStatus) (b :: FieldStatus) :: FieldStatus where
  Conjunction 'FieldOn 'FieldOn = 'FieldOn
  Conjunction a        b        = 'FieldOff

type family Disjunction (a :: FieldStatus) (b :: FieldStatus) :: FieldStatus where
  Disjunction 'FieldOff 'FieldOff = 'FieldOff
  Disjunction a         b         = 'FieldOn

type family Exclusive (a :: FieldStatus) (b :: FieldStatus) :: FieldStatus where
  Exclusive 'FieldOn  'FieldOn  = 'FieldOff
  Exclusive 'FieldOff 'FieldOff = 'FieldOff
  Exclusive a         b         = 'FieldOn

type family Negate (a :: FieldStatus) :: FieldStatus where
  Negate 'FieldOn  = 'FieldOff
  Negate 'FieldOff = 'FieldOn

type family GetFieldStatus (s :: SubsetSelector l) (p :: [l]) :: FieldStatus where
  -- Constants
  GetFieldStatus 'FullSet any            = 'FieldOn
  GetFieldStatus 'EmptySet any           = 'FieldOff
  -- Set operations
  GetFieldStatus (Union a b) ss          = Disjunction (GetFieldStatus a ss) (GetFieldStatus b ss)
  GetFieldStatus (Intersection a b) ss   = Conjunction (GetFieldStatus a ss) (GetFieldStatus b ss)
  GetFieldStatus (Difference a b) ss     = Conjunction (GetFieldStatus a ss) (Negate (GetFieldStatus b ss))
  GetFieldStatus (SymDiff a b) ss        = Exclusive (GetFieldStatus a ss) (GetFieldStatus b ss)
  -- Linear search
  GetFieldStatus ('Subset s) (s ': rest) = 'FieldOn
  GetFieldStatus s (x ': rest)           = GetFieldStatus s rest
  GetFieldStatus s '[]                   = 'FieldOff

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

(.:|?) :: (Applicative (FieldWrapper s), Aeson.FromJSON v)
       => Aeson.Object -> T.Text -> Aeson.Parser (FieldWrapper s (Maybe v))
o .:|? k = pure <$> o Aeson..:? k

--------------------------------------------------------------------------------
-- Lens
--------------------------------------------------------------------------------

-- | A lens that focuses the value inside an active field wrapper.
fieldLens :: Functor f
          => (a -> f b)
          -> FieldWrapper 'FieldOn a
          -> f (FieldWrapper 'FieldOn b)
fieldLens f (Wrapped a) = Wrapped <$> f a

