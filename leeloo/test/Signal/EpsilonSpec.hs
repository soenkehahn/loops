
module Signal.EpsilonSpec where

import Test.Hspec
import Prelude hiding (floor)
import Signal.Epsilon

spec :: Spec
spec = do
  describe "floor" $ do
    it "returns the same number if given an integer" $ do
      floor (3 :: Double) `shouldBe` 3

    it "returns the lower integer, when given a fraction closer to the lower integer" $ do
      floor (3.3 :: Double) `shouldBe` 3

    it "returns the lower integer when given a fraction closer to the upper integer" $ do
      floor (3.7 :: Double) `shouldBe` 3

    it "returns the upper integer when less than epsilon below it" $ do
      floor (4 - (epsilon / 2) :: Double) `shouldBe` 4
