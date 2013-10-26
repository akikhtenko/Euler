module Kikhteal.QuickCheck where

import Test.QuickCheck
import Control.Monad

newtype SmallPositive = SmallPositive Integer deriving (Eq,Show)
instance Arbitrary SmallPositive where
  arbitrary = liftM SmallPositive (choose (1,1000))

newtype SmallPositiveList = SmallPositiveList [Integer] deriving (Eq,Show)
instance Arbitrary SmallPositiveList where
  arbitrary = sized $ \s -> do
                 n <- choose (0,s `min` 10)
                 xs <- vectorOf n (choose (1,1000))
                 return (SmallPositiveList xs)