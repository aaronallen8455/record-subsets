{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import           Data.Aeson
import           Data.Aeson.Types
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as HH
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Data.RecordSubset

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "JSON"
  [ testProperty "Can round-trip through JSON" . HH.property $ do
      f <- HH.forAll (fooGen :: HH.Gen (Foo FullSet))
      HH.tripping f encode decode
      a <- HH.forAll (fooGen :: HH.Gen (Foo (Subset 'A)))
      HH.tripping a encode decode
      b <- HH.forAll (fooGen :: HH.Gen (Foo (Subset 'A)))
      HH.tripping b encode decode
      c <- HH.forAll (fooGen :: HH.Gen (Foo (Subset 'A)))
      HH.tripping c encode decode
      d <- HH.forAll (fooGen :: HH.Gen (Foo (Subset 'A)))
      HH.tripping d encode decode
      e <- HH.forAll (fooGen :: HH.Gen (Foo EmptySet))
      HH.tripping e encode decode
  ]

data SetId = A | B | C | D

data Foo (s :: SubsetSelector SetId) =
  Foo
    { foo1 :: SubsetField String s '[ 'A, 'B]
    , foo2 :: SubsetField Bool s '[ 'C, 'D]
    , foo3 :: SubsetField Bar s '[ 'A, 'D]
    } deriving (Show, Eq)

type FooA = Foo (Subset 'A)
type FooB = Foo (Subset 'B)
type FooC = Foo (Subset 'C)
type FooD = Foo (Subset 'D)

data Bar =
  Bar
    { bar1 :: Int
    , bar2 :: Maybe Int
    } deriving (Show, Eq)

instance ToJSON (Foo s) where
  toJSON f =
    objectSubset
    [ "foo1" .=| foo1 f
    , "foo2" .=| foo2 f
    , "foo3" .=| foo3 f
    ]

fooParser :: _ => Value -> Parser (Foo s)
fooParser = withObject "Foo" $ \o ->
  Foo <$> o .:| "foo1"
      <*> o .:| "foo2"
      <*> o .:| "foo3"

instance FromJSON (Foo FullSet) where
  parseJSON = fooParser

instance FromJSON (Foo (Subset 'A)) where
  parseJSON = fooParser

instance FromJSON (Foo (Subset 'B)) where
  parseJSON = fooParser

instance FromJSON (Foo (Subset 'C)) where
  parseJSON = fooParser

instance FromJSON (Foo (Subset 'D)) where
  parseJSON = fooParser

instance FromJSON (Foo EmptySet) where
  parseJSON = fooParser

instance ToJSON Bar where
  toJSON b =
    object
    [ "bar1" .= bar1 b
    , "bar2" .= bar2 b
    ]

instance FromJSON Bar where
  parseJSON = withObject "Bar" $ \o ->
    Bar <$> o .: "bar1"
        <*> o .: "bar2"

fieldWrapperGen :: Applicative (FieldWrapper s) => HH.Gen a -> HH.Gen (FieldWrapper s a)
fieldWrapperGen = fmap pure

fooGen :: _
           => HH.Gen (Foo s)
fooGen = Foo
   <$> fieldWrapperGen (Gen.string (HH.constant 0 10) Gen.latin1)
   <*> fieldWrapperGen Gen.enumBounded
   <*> fieldWrapperGen barGen

barGen :: HH.Gen Bar
barGen = Bar
  <$> Gen.integral (HH.constant minBound maxBound)
  <*> Gen.maybe (Gen.integral $ HH.constant minBound maxBound)

--fooAGen :: HH.Gen FooA
--fooAGen = Foo
--     <$>
--     <*>
--
