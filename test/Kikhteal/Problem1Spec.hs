module Kikhteal.Problem1Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Kikhteal.Problem1
import Test.QuickCheck
import Data.Maybe

spec :: Spec
spec = do
  describe "multiplesOf3And5Sum" $ do
    it "results in Nothing given invalid limit" $ do
        multiplesSum 0 [3, 5] `shouldBe` Nothing

    it "results in Nothing given empty list of nums" $ do
        multiplesSum 10 [] `shouldBe` Nothing

    it "results in 33 for all multiples of 3 and 5 less than 11" $ do
        multiplesSum 10 [3, 5] `shouldBe` Just 33

    prop "never exceeds all list elements sum" $ do
      forAll (choose (1,1000)) $ \x ->
        forAll (sized $ \s -> choose (1, s `min` 10) >>= \k -> vectorOf k (choose (1,1000))) $ \ys -> 
          not (null ys) ==> fromJust (multiplesSum x ys) <= sum [1..x]