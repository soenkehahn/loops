module SignalSpec where

import Test.Hspec
import Signal
import Prelude ()
import Test.HUnit (assertFailure)
import Data.String.Conversions

shouldYield :: (Show a, Epsilon a) => Signal a -> [a] -> IO ()
shouldYield signal expected = do
  test 0.5 1.5 signal expected

test :: (HasCallStack, Epsilon a, Show a) => Double -> Double -> Signal a -> [a] -> IO ()
test delta length signal expected =
  toList delta (take length signal) `shouldBeCloseTo` expected

shouldBeCloseTo :: (HasCallStack, Epsilon a, Show a) => a -> a -> IO ()
shouldBeCloseTo got expected =
  if got ==== expected
    then return ()
    else assertFailure $
      "expected: " ++ show expected ++ "\n but got: " ++ show got

class Epsilon a where
  (====) :: a -> a -> Bool

instance Epsilon a => Epsilon [a] where
  as ==== bs = case (as, bs) of
    (a : ar, b : br) ->
      if a ==== b then ar ==== br else False
    ([], []) -> True
    _ -> False

instance Epsilon Double where
  a ==== b = abs (a - b) < 0.000000001

instance Epsilon Integer where
  (====) = (==)

spec :: Spec
spec = do
  describe "take" $ do
    it "takes samples for the given amount of time" $ do
      constant 42 `shouldYield` [42, 42, 42 :: Integer]

  describe "fromList" $ do
    it "converts a list into a signal" $ do
      fromList [1, 2, 3] `shouldYield` [1, 2, 3 :: Integer]

  describe "toByteString" $ do
    it "converts the numbers to a lazy ByteString, one per line" $ do
      toByteString 1 (fromList [1, 2, 3 :: Double]) `shouldBe` cs "1\n2\n3\n"

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

  describe "shift" $ do
    it "omits the beginning of the signal, when the shift value is negative" $ do
      let signal = shift (-0.2) $ take 1 saw
      test 0.1 10 signal [-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, -1]

    it "inserts silence, when the shift value is positive" $ do
      let signal = shift 0.2 $ take 0.5 saw
      test 0.1 10 signal [0, 0, -1, -0.8, -0.6, -0.4, -0.2, 0]

  describe "ramp" $ do
    it "allows to specify a ramp" $ do
      let signal = ramp 1 0.3 1.3
      test 0.1 2 signal [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2 :: Double]

    it "allows to specify a ramp with negative slope" $ do
      let signal = ramp 1 1.3 0.3
      test 0.1 2 signal [1.3, 1.2, 1.1, 1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3 :: Double]

    it "allows to specify the length of the ramp" $ do
      let signal = ramp 0.5 0.3 1.3
      test 0.1 2 signal [0.3, 0.5, 0.7, 0.9, 1.1, 1.3 :: Double]

  describe "adsr" $ do
    it "allows to have a attack" $ do
      test 0.1 2 (adsr 1 (Adsr 0.3 0 1 0) (constant 10)) [0, 10 / 3, 10 * 2 / 3, 10, 10, 10, 10, 10, 10, 10, 10]

    it "allows to have a release" $ do
      test 0.1 2 (adsr 1 (Adsr 0 0 1 0.3) (constant 10)) [10, 10, 10, 10, 10, 10, 10, 10, 10, 10 * 2 / 3, 10 / 3]

    it "allows to have decay and sustain" $ do
      test 0.1 2 (adsr 1 (Adsr 0 0.2 0.5 0) (constant 10)) [10, 7.5, 5, 5, 5, 5, 5, 5, 5, 5, 5]

    it "release ramp starts at sustain volume" $ do
      test 0.1 2 (adsr 1 (Adsr 0 0.2 0.5 0.3) (constant 10)) [10, 7.5, 5, 5, 5, 5, 5, 5, 5, 5 * 2 / 3, 5 / 3]
