module SignalSpec where

import Test.Hspec
import Signal
import Prelude ()

shouldYield :: (Show a, Eq a) => Signal a -> [a] -> IO ()
shouldYield signal expected = do
  test 0.5 1.5 signal expected

test :: (HasCallStack, Show a, Eq a) => Double -> Double -> Signal a -> [a] -> IO ()
test delta length signal expected =
  toList delta (take length signal) `shouldBe` expected

spec :: Spec
spec = do
  describe "take" $ do
    it "takes samples for the given amount of time" $ do
      constant 42 `shouldYield` [42, 42, 42]

  describe "fromList" $ do
    it "converts a list into a signal" $ do
      fromList [1, 2, 3] `shouldYield` [1, 2, 3]

  describe "phase" $ do
    it "ramps up to TAU in one second" $ do
      phase `shouldYield` [0, tau / 2, 0]

    it "wraps around after one second" $ do
      test 0.25 1.5 phase [0, tau / 4, tau / 2, 3 * tau / 4, 0, tau / 4]

  describe "project" $ do
    it "converts values from one range to another" $ do
      map (project (0, 100) (-1, 1)) [0, 50, 100]
        `shouldBe` [-1, 0, 1]

  describe "speedup" $ do
    it "speeds up another signal" $ do
      test 0.25 1 (speedup (constant 2) phase) [0, tau / 2, 0, tau / 2]

  describe "applicative interface" $ do
    it "<*> and <$> work" $ do
      ((+) <$> constant 3 <*> constant 4)
        `shouldYield` [7, 7, 7]

  it "allows to easily use an lfo" $ do
    let lfo = fmap (project (-1, 1) (300, 400) . sin) phase
        signal = speedup lfo $ fmap sin phase
    signal `shouldYield` [0.0, -2.1561211432632476e-14, -2.1561211432632476e-14]

  describe "|>" $ do
    it "allows to sequentialize signals" $ do
      let signal = take 0.5 (constant 23) |> take 0.5 (constant 42)
      test 0.25 3 signal [23, 23, 42, 42]

  describe "repeat" $ do
    it "repeats a signal n times" $ do
      test 1 10 (repeat 3 (take 1 (constant 42))) [42, 42, 42]

  describe "+++" $ do
    it "adds two signals" $ do
      let signal = constant 23 +++ constant 42
      test 1 3 signal [65, 65, 65]

    it "keeps the first signal if the second stops" $ do
      let signal = constant 23 +++ take 1 (constant 42)
      test 1 3 signal [65, 23, 23]

    it "keeps the second signal if the first stops" $ do
      let signal = take 1 (constant 23) +++ constant 42
      test 1 3 signal [65, 42, 42]

  describe "silence" $ do
    it "returns silences of the given length" $ do
      test 1 4 (silence 3) [0, 0, 0]

  describe "fill" $ do
    it "plays back the given signal but fills the rest with silence" $ do
      let signal = fill 2 (take 1 (constant 42))
      test 0.5 4 signal [42, 42, 0, 0]
