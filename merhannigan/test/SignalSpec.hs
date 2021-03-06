module SignalSpec where

import Song ()
import Prelude ()
import Signal
import Signal.Core
import Test.Hspec hiding (focus)
import Test.Utils
import qualified Data.Vector.Storable as Vec

spec :: Spec
spec = do
  describe "fromList" $ do
    it "converts a list into a signal" $ do
      fromList 0.5 [1, 2, 3] `shouldYield` [1, 2, 3 :: Integer]

  describe "take" $ do
    it "takes samples for the given amount of time" $ do
      constant 42 `shouldYield` [42, 42, 42 :: Integer]

    it "handles shorter input signals correctly" $ do
      let signal = take (take (constant 42) 1) 2
      end signal `shouldBeCloseTo` Finite 1
      signal `shouldYield` [42, 42 :: Double]

  describe "skip" $ do
    it "skips the first part of a signal" $ do
      let signal = skip 0.2 $ ramp 0 1 1
      test 0.1 2 signal [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]

  describe "focus" $ do
    it "focusses on the given time window" $ do
      let signal = focus 0.2 0.5 $ ramp 0 1 1
      test 0.1 2 signal [0.2, 0.3, 0.4, 0.5, 0.6]

  describe "zip" $ do
    it "zips two signals together" $ do
      let signal = zip (constant 23) (constant 42)
      test 0.5 1 signal [(23, 42), (23, 42) :: (Double, Double)]

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

  describe "constSpeedup" $ do
    it "speeds the signal up by a constant" $ do
      let signal = constSpeedup 2 $ ramp 0 1 1
      end signal `shouldBeCloseTo` Finite 0.5
      test 0.1 10 signal [0, 0.2, 0.4, 0.6, 0.8 :: Double]

  describe "speedup" $ do
    it "speeds up another signal" $ do
      test 0.25 1 (speedup (constant 2) phase) [0, tau / 2, 0, tau / 2]

  describe "integral" $ do
    it "computes the integral with a constant of 0" $ do
      let signal = integral (fromList 1 [0, 3, 5, 7])
      test 1 10 signal [0, 3, 8, 15 :: Double]

    it "takes varying time deltas into account" $ do
      let signal = integral (fromList 1 [0, 3, 5])
      runOnTimes signal (Vec.fromList [0, 1.2, 1.7, 2.2])
        `shouldBeCloseTo` Vec.fromList [0, 3.6, 5.1, 7.6]

    it "always starts with 0" $ do
      test 1 1 (integral (constant 42)) [0 :: Double]

  it "allows to easily use an lfo" $ do
    let lfo = fmap (project (-1, 1) (300, 400) . sin) phase
        signal = speedup lfo $ fmap sin phase
    signal `shouldYield` [0.0, -2.1561211432632476e-14, -2.1561211432632476e-14]

  describe "|>" $ do
    it "allows to sequentialize signals" $ do
      let signal = take (constant 23) 0.5 |> take (constant 42) 0.5
      test 0.25 3 signal ([23, 23, 42, 42] :: [Integer])

    it "passes in the right time to the second snippet" $ do
      let signal = ramp 0 1 1 |> ramp 0 1 1
      test 0.5 10 signal [0, 0.5, 0, 0.5 :: Double]

  describe "repeat" $ do
    it "repeats a signal n times" $ do
      test 1 10 (repeat 3 (take (constant 42) 1)) [42, 42, 42 :: Integer]

  describe "cycle" $ do
    it "repeats a signal infinitely" $ do
      let signal = cycle (ramp 0 1 1)
      test 0.5 3 signal [0, 0.5, 0, 0.5, 0, 0.5]
      end signal `shouldBeCloseTo` Infinite

    it "works for infinite signals" $ do
      let signal = cycle (ramp 0 1 1 |> constant 23)
      test 0.5 2 signal [0, 0.5, 23, 23]

  describe "+++" $ do
    it "adds two signals" $ do
      let signal = constant 23 +++ constant 42
      test 1 3 signal [65, 65, 65 :: Double]

    it "keeps the first signal if the second stops" $ do
      let signal = constant 23 +++ take (constant 42) 1
      test 1 3 signal [65, 23, 23 :: Double]

    it "keeps the second signal if the first stops" $ do
      let signal = take (constant 23) 1 +++ constant 42
      test 1 3 signal [65, 42, 42 :: Double]

  describe "mix" $ do
    it "mixes a list of signals with +++" $ do
      let signal = mix [constant 23, take (constant 42) 1]
      test 1 3 signal [65, 23, 23 :: Double]

  describe "mixWithVolumes" $ do
    it "mixes signals according to their given volume" $ do
      let signal = mixWithVolumes $
            (2, constant (23 :: Double)) :
            (3, constant 42) :
            []
      getSample signal 0 `shouldBe` 2 * 23 + 3 * 42

  describe "/\\" $ do
    it "multiplies two signals" $ do
      constant 3 /\ constant 4 `shouldYield` [12, 12, 12 :: Double]

    it "stops when the first signal stops" $ do
      let signal = take (constant 3) 1 /\ constant 4
      test 1 3 signal [12 :: Double]

    it "stops when the second signal stops" $ do
      let signal = constant 3 /\ take (constant 4) 1
      test 1 3 signal [12 :: Double]

    it "binds tighter than |>" $ do
      let signal :: Signal Double
          signal = constant 3 /\ take (constant 4) 1 |> take (constant 5) 1
      test 1 10 signal [12, 5]

  describe "silence" $ do
    it "returns silences of the given length" $ do
      test 1 4 (silence 3) [0, 0, 0 :: Integer]

  describe "withEnd" $ do
    it "plays back the given signal but fills the rest with silence" $ do
      let signal = withEnd 2 (take (constant 42) 1)
      test 0.5 4 signal [42, 42, 0, 0 :: Integer]

  describe "ramp" $ do
    it "allows to specify a ramp" $ do
      let signal = ramp 0.3 1.3 1
      test 0.1 2 signal [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2 :: Double]

    it "allows to specify a ramp with negative slope" $ do
      let signal = ramp 1.3 0.3 1
      test 0.1 2 signal [1.3, 1.2, 1.1, 1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4 :: Double]

    it "allows to specify the length of the ramp" $ do
      let signal = ramp 0.3 1.3 0.5
      test 0.1 2 signal [0.3, 0.5, 0.7, 0.9, 1.1:: Double]

  describe "adsr" $ do
    it "allows to have an attack" $ do
      test 0.1 2 (adsr 1 (Adsr 0.3 0 1 0))
        [0, 1 / 3, 1 * 2 / 3, 1, 1, 1, 1, 1, 1, 1]

    it "allows to have a release after the length" $ do
      test 0.1 2 (adsr 1 (Adsr 0 0 1 0.3))
        [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 * 2 / 3, 1 / 3]

    it "allows to have decay and sustain" $ do
      test 0.1 2 (adsr 1 (Adsr 0 0.2 0.5 0))
        [1, 0.75, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]

    it "release ramp starts at sustain volume" $ do
      test 0.1 2 (adsr 1 (Adsr 0 0.2 0.5 0.3))
        [1, 0.75, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 * 2 / 3, 0.5 / 3]

    it "crashes when the sum of attack and decay is longer then the given length" $ do
      print (getSample (adsr 1 (Adsr 0.5 0.7 0 0.8)) 0) `shouldThrow`
        errorCall "(Adsr 0.5 0.7 0.0 0.8) requires a length longer than 1.2, given length: 1.0"

    it "doesn't include the release in the given length" $ do
      getSample (adsr 1 (Adsr 0.4 0.4 0 0.4)) 0 `shouldBeCloseTo` 0

  describe "clip" $ do
    it "limits the signal at the upper limit" $ do
      clip (0, 5) 7 `shouldBe` 5

    it "limits the signal at the lower limit" $ do
      clip (0, 5) (-1) `shouldBe` 0

    it "passes the signal through when within limits" $ do
      clip (0, 5) 3 `shouldBe` 3
