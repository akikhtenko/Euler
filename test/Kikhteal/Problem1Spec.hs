module Kikhteal.Problem1Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Kikhteal.Problem1
import Kikhteal.QuickCheck

spec :: Spec
spec = do
  describe "multiplesOf3And5Sum" $ do
    prop "never exceeds all list elements sum" $ do
      \(SmallPositive x) (SmallPositiveList ys) -> 
        multiplesSum x ys <= sum [1..x]
    
    it "results in 33 for all multiples of 3 and 5 less than 11" $ do
        multiplesSum 10 [3, 5] `shouldBe` 33