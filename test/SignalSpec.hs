module SignalSpec where

import Test.Hspec
import Signal
import Prelude hiding (take)

shouldYield :: (Show a, Eq a) => Signal a -> [a] -> IO ()
shouldYield signal expected = do
  test 0.5 1 signal expected

test :: (Show a, Eq a) => Double -> Double -> Signal a -> [a] -> IO ()
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
      test 0.25 1.25 phase [0, tau / 4, tau / 2, 3 * tau / 4, 0, tau / 4]

  describe "project" $ do
    it "converts values from one range to another" $ do
      map (project (0, 100) (-1, 1)) [0, 50, 100]
        `shouldBe` [-1, 0, 1]

  describe "speedup" $ do
    it "speeds up another signal" $ do
      test 0.25 1 (speedup (constant 2) phase) [0, tau / 2, 0, tau / 2, 0]

  describe "applicative interface" $ do
    it "<*> and <$> work" $ do
      ((+) <$> constant 3 <*> constant 4)
        `shouldYield` [7, 7, 7]

  it "allows to easily use an lfo" $ do
    let lfo = fmap (project (-1, 1) (300, 400) . sin) phase
        signal = speedup lfo $ fmap sin phase
    signal `shouldYield` [0.0, -2.1561211432632476e-14, -2.1561211432632476e-14]
