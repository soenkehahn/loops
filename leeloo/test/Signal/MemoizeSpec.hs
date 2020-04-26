module Signal.MemoizeSpec where

import Test.Hspec
import Signal
import Signal.Memoize
import Test.Utils

spec :: Spec
spec = do
  describe "_toTime" $ do
    it "maps 0 to 0" $ do
      _toTime 10 7 0 `shouldBeCloseTo` 0

    it "maps the last index to something close to but below the time length" $ do
      _toTime 10 100 99 `shouldBeCloseTo` 9.9

    it "maps all indices to equal distances from each other" $ do
      map (_toTime 10 4) [0 .. 3] `shouldBeCloseTo` [0, 2.5, 5, 7.5]

  describe "_toIndex" $ do
    it "maps 0 to the first cell" $ do
      _toIndex 10 100 0 `shouldBe` 0

    it "maps something close to but below the time length to the last cell" $ do
      _toIndex 10 100 9.95 `shouldBe` 99

    it "maps times in between values to the floor cell" $ do
      _toIndex 10 7 5 `shouldBe` 3

  describe "memoize" $ do
    it "returns the same values as the input signal" $ do
      let signal = memoize 4 $ ramp 0 1 1
      test 0.25 1 signal [0, 0.25, 0.5, 0.75]

    it "interpolates values by rounding to the nearest lower index" $ do
      let signal = memoize 3 $ ramp 0 1 1
      getSample signal 0.5 `shouldBeCloseTo` (1 / 3)

    it "works for empty signals" $ do
      let signal :: Signal Double
          signal = memoize 3 empty
      signalLength signal `shouldBeCloseTo` Finite 0

    it "throws an exception for infinite signals" $ do
      print (signalLength (memoize 3 (constant (0 :: Double)))) `shouldThrow`
        errorCall "memoize not implemented for infinite signals"
