{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import qualified Hedgehog as HH
import           Hedgehog ((===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as HH
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Data.RecordSubset

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Subsets"
  [ testProperty "Matches expectations" . HH.property $ do
      a <- HH.forAll (fooGen :: HH.Gen FooA)
      let (Wrapped _x) = foo1 a
      foo2 a === Nil
      let (Wrapped _x) = foo3 a
      let (Wrapped _x) = foo4 a

      b <- HH.forAll (fooGen :: HH.Gen FooB)
      let (Wrapped _x) = foo1 b
      foo2 b === Nil
      foo3 b === Nil
      let (Wrapped _x) = foo4 b

      c <- HH.forAll (fooGen :: HH.Gen FooC)
      foo1 c === Nil
      let (Wrapped _x) = foo2 c
      foo3 c === Nil
      foo4 c === Nil

      d <- HH.forAll (fooGen :: HH.Gen FooD)
      foo1 d === Nil
      let (Wrapped _x) = foo2 d
      let (Wrapped _x) = foo3 d
      let (Wrapped _x) = foo4 d

      e <- HH.forAll (fooGen :: HH.Gen (Foo EmptySet))
      foo1 e === Nil
      foo2 e === Nil
      foo3 e === Nil
      foo4 e === Nil

      f <- HH.forAll (fooGen :: HH.Gen (Foo (Subset 'A `Union` Subset 'D)))
      let (Wrapped _x) = foo1 f
      let (Wrapped _x) = foo2 f
      let (Wrapped _x) = foo3 f
      let (Wrapped _x) = foo4 f

      g <- HH.forAll (fooGen :: HH.Gen (Foo (Subset 'A `Intersection` Subset 'B)))
      let (Wrapped _x) = foo1 g
      foo2 g === Nil
      foo3 g === Nil
      let (Wrapped _x) = foo4 g

      h <- HH.forAll (fooGen :: HH.Gen (Foo (Subset 'A `Difference` Subset 'B)))
      foo1 h === Nil
      foo2 h === Nil
      let (Wrapped _x) = foo3 h
      foo4 h === Nil

      i <- HH.forAll (fooGen :: HH.Gen (Foo (Subset 'A `SymDiff` Subset 'D)))
      let (Wrapped _x) = foo1 i
      let (Wrapped _x) = foo2 i
      foo3 i === Nil
      foo4 i === Nil
  ]

data SetId = A | B | C | D

data Foo (s :: SubsetSelector SetId) =
  Foo
    { foo1 :: SubsetField String s '[ 'A, 'B]
    , foo2 :: SubsetField Bool s '[ 'C, 'D]
    , foo3 :: SubsetField Bar s '[ 'A, 'D]
    , foo4 :: SubsetField (Maybe (Baz s)) s '[ 'A, 'B, 'D]
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

data Baz (s :: SubsetSelector SetId) =
  Baz
    { baz1 :: SubsetField String s '[ 'A, 'D]
    , baz2 :: SubsetField Int s '[ 'B ]
    } deriving (Show, Eq)

fieldWrapperGen :: Applicative (FieldWrapper s) => HH.Gen a -> HH.Gen (FieldWrapper s a)
fieldWrapperGen = fmap pure

fooGen :: _ => HH.Gen (Foo s)
fooGen = Foo
   <$> fieldWrapperGen (Gen.string (HH.constant 0 10) Gen.latin1)
   <*> fieldWrapperGen Gen.enumBounded
   <*> fieldWrapperGen barGen
   <*> fieldWrapperGen (Gen.maybe bazGen)

barGen :: HH.Gen Bar
barGen = Bar
  <$> Gen.integral (HH.constant minBound maxBound)
  <*> Gen.maybe (Gen.integral $ HH.constant minBound maxBound)

bazGen :: _ => HH.Gen (Baz s)
bazGen = Baz
     <$> fieldWrapperGen (Gen.string (HH.constant 0 10) Gen.latin1)
     <*> fieldWrapperGen (Gen.integral $ HH.constant (-100) 100)

