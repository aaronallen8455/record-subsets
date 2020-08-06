{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Main where

import           Data.RecordSubset

main :: IO ()
main = pure ()

data SubsetId = A | B | C | D

data Foo (t :: SubsetSelector SubsetId) =
  Foo
    { foo1 :: SubsetField String t       '[ 'A     ]
    , foo2 :: SubsetField Int t          '[ 'A, 'B ]
    , foo3 :: SubsetField (Maybe Bool) t '[ 'C, 'B ]
    } deriving (Show, Eq)

selectSubset :: _ => Foo FullSet -> Foo subSet
selectSubset
  = Foo <$> pure . getField . foo1
        <*> pure . getField . foo2
        <*> pure . getField . foo3

data Bar =
  Bar { bar1 :: String
      , bar2 :: Int
      , bar3 :: Maybe Bool
      } deriving Show

-- Good to hide this constraint?
barToFoo :: _ => Bar -> Foo subSet
barToFoo b =
  Foo { foo1 = pure $ bar1 b
      , foo2 = pure $ bar2 b
      , foo3 = pure $ bar3 b
      }

bar :: Bar
bar =
  Bar
    { bar1 = "some string"
    , bar2 = 7
    , bar3 = Just False
    }

