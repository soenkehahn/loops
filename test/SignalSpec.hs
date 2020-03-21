module SignalSpec where

import Loop ()
import Test.Hspec
import Signal
import Prelude ()
import Data.String.Conversions
import Test.Utils
import qualified Data.Vector as Vec

shouldYield :: (Show a, Epsilon a) => Signal a -> [a] -> IO ()
shouldYield signal expected = do
  test 0.5 1.5 signal expected

spec :: Spec
spec = do
  describe "fromList" $ do
    it "converts a list into a signal" $ do
      fromList 0.5 [1, 2, 3] `shouldYield` [1, 2, 3 :: Integer]

  describe "toByteString" $ do
    it "converts the numbers to a lazy ByteString, one per line" $ do
      toByteString 1 (fromList 1 [1, 2, 3 :: Double]) `shouldBe` cs "1\n2\n3\n"

  describe "take" $ do
    it "takes samples for the given amount of time" $ do
      constant 42 `shouldYield` [42, 42, 42 :: Integer]

  describe "skip" $ do
    it "skips the first part of a signal" $ do
      let signal = skip 0.2 $ ramp 1 0 1
      test 0.1 2 signal [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]

  describe "phase" $ do
    it "ramps up to TAU in one second" $ do
      phase `shouldYield` [0, tau / 2, 0]

    it "wraps around after one second" $ do
      test 0.25 1.5 phase [0, tau / 4, tau / 2, 3 * tau / 4, 0, tau / 4]

  describe "rect" $ do
    it "is a rectangle wave" $ do
      test 0.25 1.5 rect [-1, -1, 1, 1, -1, -1]

  describe "project" $ do
    it "converts values from one range to another" $ do
      map (project (0, 100) (-1, 1)) [0, 50, 100]
        `shouldBe` [-1, 0, 1]

  describe "speedup" $ do
    it "speeds up another signal" $ do
      test 0.25 1 (speedup (constant 2) phase) [0, tau / 2, 0, tau / 2]

  describe "integral" $ do
    it "computes the integral with a constant of 0" $ do
      integral (Vec.fromList [(0, 0), (1, 3), (2, 5), (3, 7)]) `shouldBe`
        Vec.fromList [0, 3, 8, 15 :: Double]

    it "takes smaller time deltas into account" $ do
      integral (Vec.fromList [(0, 0), (0.1, 3), (0.3, 5), (1, 7)]) `shouldBeCloseTo`
        Vec.fromList [0, 0.3, 1.3, 6.2 :: Double]

  describe "applicative interface" $ do
    it "<*> and <$> work" $ do
      ((+) <$> constant 3 <*> constant 4)
        `shouldYield` [7, 7, 7 :: Integer]

  it "allows to easily use an lfo" $ do
    let lfo = fmap (project (-1, 1) (300, 400) . sin) phase
        signal = speedup lfo $ fmap sin phase
    signal `shouldYield` [0.0, -2.1561211432632476e-14, -2.1561211432632476e-14]

  describe "|>" $ do
    it "allows to sequentialize signals" $ do
      let signal = take 0.5 (constant 23) |> take 0.5 (constant 42)
      test 0.25 3 signal ([23, 23, 42, 42] :: [Integer])

  describe "repeat" $ do
    it "repeats a signal n times" $ do
      test 1 10 (repeat 3 (take 1 (constant 42))) [42, 42, 42 :: Integer]

  describe "+++" $ do
    it "adds two signals" $ do
      let signal = constant 23 +++ constant 42
      test 1 3 signal [65, 65, 65 :: Double]

    it "keeps the first signal if the second stops" $ do
      let signal = constant 23 +++ take 1 (constant 42)
      test 1 3 signal [65, 23, 23 :: Double]

    it "keeps the second signal if the first stops" $ do
      let signal = take 1 (constant 23) +++ constant 42
      test 1 3 signal [65, 42, 42 :: Double]

  describe "/\\" $ do
    it "multiplies two signals" $ do
      constant 3 /\ constant 4 `shouldYield` [12, 12, 12 :: Double]

    it "stops when the first signal stops" $ do
      let signal = take 1 (constant 3) /\ constant 4
      test 1 3 signal [12 :: Double]

    it "stops when the second signal stops" $ do
      let signal = constant 3 /\ take 1 (constant 4)
      test 1 3 signal [12 :: Double]

  describe "silence" $ do
    it "returns silences of the given length" $ do
      test 1 4 (silence 3) [0, 0, 0 :: Integer]

  describe "fill" $ do
    it "plays back the given signal but fills the rest with silence" $ do
      let signal = fill 2 (take 1 (constant 42))
      test 0.5 4 signal [42, 42, 0, 0 :: Integer]

  describe "ramp" $ do
    it "allows to specify a ramp" $ do
      let signal = ramp 1 0.3 1.3
      test 0.1 2 signal [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2 :: Double]

    it "allows to specify a ramp with negative slope" $ do
      let signal = ramp 1 1.3 0.3
      test 0.1 2 signal [1.3, 1.2, 1.1, 1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4 :: Double]

    it "allows to specify the length of the ramp" $ do
      let signal = ramp 0.5 0.3 1.3
      test 0.1 2 signal [0.3, 0.5, 0.7, 0.9, 1.1:: Double]

  describe "adsr" $ do
    it "allows to have a attack" $ do
      test 0.1 2 (adsr 1 (Adsr 0.3 0 1 0) (constant 10)) [0, 10 / 3, 10 * 2 / 3, 10, 10, 10, 10, 10, 10, 10]

    it "allows to have a release" $ do
      test 0.1 2 (adsr 1 (Adsr 0 0 1 0.3) (constant 10)) [10, 10, 10, 10, 10, 10, 10, 10, 10 * 2 / 3, 10 / 3]

    it "allows to have decay and sustain" $ do
      test 0.1 2 (adsr 1 (Adsr 0 0.2 0.5 0) (constant 10)) [10, 7.5, 5, 5, 5, 5, 5, 5, 5, 5]

    it "release ramp starts at sustain volume" $ do
      test 0.1 2 (adsr 1 (Adsr 0 0.2 0.5 0.3) (constant 10)) [10, 7.5, 5, 5, 5, 5, 5, 5, 5 * 2 / 3, 5 / 3]

  describe "clip" $ do
    it "limits the signal at the upper limit" $ do
      clip (0, 5) 7 `shouldBe` 5

    it "limits the signal at the lower limit" $ do
      clip (0, 5) (-1) `shouldBe` 0

    it "passes the signal through when within limits" $ do
      clip (0, 5) 3 `shouldBe` 3
