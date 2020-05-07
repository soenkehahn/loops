module Signal.UtilsSpec where

import Test.Hspec
import Signal.Utils
import Signal
import Test.Utils

spec :: Spec
spec = do
  describe "fanOut" $ do
    it "applies the given function to every element and adds the results" $ do
      let signal = fanOut [1.0, 0.2, 0.03] (\ x -> constant x)
      test 1 1 signal [1.23 :: Double]

  describe "harmonics" $ do
    let testHarmonics :: [Double] -> (Double -> Double) -> IO ()
        testHarmonics weights expected = do
          test 0.1 1 (harmonics weights) (map expected [0, 0.1 .. 0.9])

    it "outputs a sine wave for one harmonic" $ do
      testHarmonics [1] $ \ t -> sin (t * tau)

    it "allows first harmonic with doubled frequency" $ do
      testHarmonics [0, 1] $ \ t -> sin (2 * t * tau)

    it "allows to mix multiple harmonics" $ do
      testHarmonics [1, 1] $ \ t ->
        sin (t * tau) +
        sin (2 * t * tau)

    it "allows to mix harmonics with different weights" $ do
      testHarmonics [2, 3] $ \ t ->
        2 * sin (t * tau) +
        3 * sin (2 * t * tau)
