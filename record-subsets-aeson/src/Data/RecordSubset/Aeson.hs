{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Data.RecordSubset.Aeson
  ( objectSubset
  , (.=|)
  , (.:|)
  , (.:|?)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T

import           Data.RecordSubset

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

