module Signal.CoreSpec where

import Test.Hspec
import Signal.Core
import Signal
import Test.Utils
import Prelude ()
import qualified Prelude
import System.IO
import System.IO.Silently
import qualified Data.Vector.Storable as Vec

spec :: Spec
spec = do
  describe "applicative interface" $ do
    it "<*> and <$> work" $ do
      ((+) <$> constant 3 <*> constant 4)
        `shouldYield` [7, 7, 7 :: Integer]

  describe "toList" $ do
    it "converts signals into vectors" $ do
      toList 0.5 (take (constant 23) 1) `shouldBe` [23, 23 :: Double]

    it "doesn't overrun the given length" $ do
      toList 0.3 (take (constant 23) 1) `shouldBe` [23, 23, 23, 23 :: Double]

    it "uses proper floating point inequalities" $ do
      toList 0.3 (take (constant 23) 0.9) `shouldBe` [23, 23, 23 :: Double]
      toList 0.2 (take (constant 23) 0.6) `shouldBe` [23, 23, 23 :: Double]

  describe "getSampleTimes" $ do
    it "returns all the sample times for the given length" $ do
      getSampleTimes 0.1 1 `shouldBeCloseTo`
        Vec.fromList [0, 0.1 .. 0.9]

    it "stays within the given length when the length is not divisible by the delta" $ do
      getSampleTimes 0.3 0.8 `shouldBeCloseTo`
        Vec.fromList [0, 0.3, 0.6]

    it "doesn't produce too many deltas on floating point rounding errors" $ do
      let end = 1.00000000000001
      getSampleTimes 0.5 end `shouldBeCloseTo` Vec.fromList [0.0, 0.5]

  describe "simpleSignal" $ do
    it "allows to turn a simple function over time into a signal" $ do
      let signal = simpleSignal $ \ time -> time * 7
      test 0.5 2 signal [0, 3.5, 7, 10.5]

    it "returns an infinite signal" $ do
      let signal = simpleSignal $ \ time -> time * 7
      end signal `shouldBeCloseTo` Infinite

  describe "printSamples" $ around_ (hSilence [stderr]) $ do
    it "prints samples in 44100 for the whole signal" $ do
      let signal = ramp 0 1 0.01
      output <- capture_ $ printSamples signal
      let outSamples = map read $ lines output
          expected = Prelude.take 441 ([0, 1 / 44100 / 0.01 ..] :: [Double])
      length outSamples `shouldBe` length expected
      outSamples `shouldBeCloseTo` expected
