{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ProductSubsets where

data SubSetId = A | B | C | D | FullSet

data FieldStatus = FieldOn | FieldOff

type family GetFieldStatus (t :: SubSetId) (p :: [SubSetId]) :: FieldStatus where
  GetFieldStatus FullSet anything = FieldOn
  GetFieldStatus t (t ': rest) = FieldOn
  GetFieldStatus t (x ': rest) = GetFieldStatus t rest
  GetFieldStatus t '[] = FieldOff

data FieldWrapper (s :: FieldStatus) a where
  Wrapped :: a -> FieldWrapper FieldOn a
  Nil     :: FieldWrapper FieldOff a

getFieldContents :: FieldWrapper FieldOn a -> a
getFieldContents (Wrapped a) = a

instance Functor (FieldWrapper b) where
  fmap f (Wrapped a) = Wrapped $ f a
  fmap _ Nil = Nil

instance Applicative (FieldWrapper FieldOff) where
  pure _ = Nil
  _ <*> _ = Nil

instance Applicative (FieldWrapper FieldOn) where
  pure a = Wrapped a
  Wrapped f <*> Wrapped a = Wrapped $ f a

instance Show a => Show (FieldWrapper b a) where
  show (Wrapped a) = show a
  show Nil = "(excluded)"

instance Eq a => Eq (FieldWrapper b a) where
  Wrapped a == Wrapped b = a == b
  Nil == Nil = True

type SubSetField a t ts = FieldWrapper (GetFieldStatus t ts) a

data Foo (t :: SubSetId) =
  Foo
    { foo1 :: SubSetField String t       '[ 'A     ]
    , foo2 :: SubSetField Int t          '[ 'A, 'B ]
    , foo3 :: SubSetField (Maybe Bool) t '[ 'C, 'B ]
    } deriving (Show, Eq)

selectSubSet :: _ => Foo 'FullSet -> Foo subSet
selectSubSet
  = Foo <$> pure . getFieldContents . foo1
        <*> pure . getFieldContents . foo2
        <*> pure . getFieldContents . foo3

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

