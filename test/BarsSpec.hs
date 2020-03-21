module BarsSpec where

import Test.Hspec
import Bars

spec :: Spec
spec = do
  describe "bars" $ do
    it "returns a list with bars up to the given maximum" $ do
      bars [4] `shouldBe` [[0], [1], [2], [3]]

    it "works for different limits" $ do
      bars [3] `shouldBe` [[0], [1], [2]]

    it "allows to generate beats" $ do
      let expected =
            [0, 0] :
            [0, 1] :
            [0, 2] :
            [0, 3] :
            [1, 0] :
            [1, 1] :
            [1, 2] :
            [1, 3] :
            [2, 0] :
            [2, 1] :
            [2, 2] :
            [2, 3] :
            [3, 0] :
            [3, 1] :
            [3, 2] :
            [3, 3] :
            []
      bars [4, 4] `shouldBe` expected

    it "allows to generate parts, bars and beats" $ do
      let expected =
            [0, 0, 0] :
            [0, 0, 1] :
            [0, 1, 0] :
            [0, 1, 1] :
            [1, 0, 0] :
            [1, 0, 1] :
            [1, 1, 0] :
            [1, 1, 1] :
            []
      bars [2, 2, 2] `shouldBe` expected
