{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.SnippetSpec where

import Prelude ()
import qualified Prelude
import Signal.Epsilon
import Test.QuickCheck
import Signal
import Signal.Snippet
import Test.Hspec
import Test.Utils

spec :: Spec
spec = do
  describe "|->" $ do
    it "allows to construct a signal" $ do
      let snippet = 0 |-> constant 42
      test 1 2 snippet [42, 42 :: Double]

    it "will delay the given signal by the given time" $ do
      let snippet = 1 |-> constant 42
      test 1 2 snippet [0, 42 :: Double]

    it "allows to add snippets together" $ do
      let snippet =
            1 |-> take 1 (constant 23) +++
            2 |-> take 1 (constant 42)
      test 1 3 snippet [0, 23, 42 :: Double]

    it "allows overlapping snippets" $ do
      let snippet =
            1 |-> take 2 (constant 23) +++
            2 |-> take 2 (constant 42)
      test 1 5 snippet [0, 23, 65, 42 :: Double]

    it "has good fixity in conjunction with |> and /\\" $ do
      let s = constant (0 :: Int)
          _snippet =
            0 |-> s |> s +++
            1 |-> s /\ s +++
            empty
      return () :: IO ()

  describe "dividing system" $ do
    let n :: Integer -> Time -> Signal Integer
        n value length =
            take length $ constant value

    describe "divide" $ do
      it "allows to divide into two halfs evenly" $ do
        let signal = divide [1 ~> n 1, 1 ~> n 2] 4
        test 1 10 signal [1, 1, 2, 2]

      it "allows to divide into uneven parts" $ do
        let signal = divide [4 ~> n 1, 1 ~> n 2] 5
        test 1 10 signal [1, 1, 1, 1, 2]

      it "allows complex patterns" $ do
        let signal = divide [3 ~> n 1, 7 ~> n 2, 2 ~> n 3, 4 ~> n 4] 16
        test 1 20 signal [1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4, 4, 4]

      it "fills with silence if parts are too short" $ do
        let signal = divide [4 ~> \ _time -> take 1 (constant 1), 1 ~> n 2] 5
        test 1 10 signal [1, 0, 0, 0, 2]

      it "handles empty input lists" $ do
        let signal = divide [] 5
        test 1 10 signal [0, 0, 0, 0, 0 :: Double]

      it "makes parts that are to long overlap into the next part" $ do
        let longer value time = take (2 * time) (constant value)
            signal = divide [1 ~> longer 1, 4 ~> n 2] 5
        test 1 10 signal [1, 3, 2, 2, 2]

    describe "evenly" $ do
      it "divides evenly" $ do
        let signal = evenly [n 1, n 2, n 3, n 4] 8
        test 1 10 signal [1, 1, 2, 2, 3, 3, 4, 4]

    describe "raster" $ do
      it "uses the given length as the time unit" $ do
        let signal = raster 2 [1 ~> n 1, 1 ~> n 2]
        test 1 10 signal [1, 1, 2, 2]

  describe "_signalVectorConfiguration" $ do
    let s _time = empty

    it "maps an even grid to vector cells" $ do
      _signalVectorConfiguration [1 ~> s, 1 ~> s, 1 ~> s] 10
        `shouldBeCloseTo` (3, 10)

    it "returns the length as the tailStart" $ do
      _signalVectorConfiguration [1 ~> s, 1 ~> s, 1 ~> s] 12
        `shouldBeCloseTo` (3, 12)

    it "maps an uneven grid to cells that correspond to the time units" $ do
      _signalVectorConfiguration [1 ~> s, 2 ~> s, 3 ~> s] 10
        `shouldBeCloseTo` (6, 10)

    it "configures a vector that is longer than the grid for grids of fractional length" $ do
      _signalVectorConfiguration [1 ~> s, 2 ~> s, 3.7 ~> s] 10
        `shouldBeCloseTo` (7, (10 / 6.7) * 7)
      _signalVectorConfiguration [1 ~> s, 2 ~> s, 3.3 ~> s] 10
        `shouldBeCloseTo` (7, (10 / 6.3) * 7)

  goldenTests

data TestSignal
  = TakeConstant Double Time
  | Ramp Double Double Time
  | Constant Double
  deriving (Show)

toSignal :: TestSignal -> Signal Double
toSignal = \case
  TakeConstant value length -> Signal (Finite length) $ do
    return $ \ time -> do
      return $ if not (time `lt` 0) && time `lt` length
        then value
        else error "boo"
  Ramp from to length ->
    ramp from to length
  Constant value ->
    constant value

instance Arbitrary TestSignal where
  arbitrary = oneof [
    TakeConstant <$> choose (-1, 1) <*> (Time <$> choose (1, 10)),
    Ramp <$> choose (-1, 1) <*> choose (-1, 1) <*> (Time <$> choose (1, 10)),
    Constant <$> choose (-1, 1)
    ]
  shrink = \case
    TakeConstant value length ->
      (TakeConstant <$> shrink value <*> pure length) ++
      (TakeConstant <$> pure value <*> map Time (shrink $ fromTime length))
    Ramp from to length ->
      (if from == to then [TakeConstant from length] else []) ++
      (Ramp <$> shrink from <*> pure to <*> pure length) ++
      (Ramp from <$> shrink to <*> pure length) ++
      (Ramp from to <$> map Time (shrink $ fromTime length))
    Constant value ->
      (Constant <$> shrink value) ++
      [TakeConstant value 10]

simpleDivide :: Num a => [Part a] -> Time -> Signal a
simpleDivide [] len = silence len
simpleDivide parts len = inner parts
  where
    timeUnit = len / Time (sum (map (realToFrac . weight) parts))
    inner parts = case parts of
      Part weight part : rest ->
        let len = (timeUnit * Time (realToFrac weight))
        in part len +++
           len |-> inner rest
      [] -> empty

goldenTests :: Spec
goldenTests = describe "property tests" $ do
  it "behaves like an unoptimized simpler version of divide" $ property $
    forAllShrink (choose (0, 100)) shrink $ \ (Time -> length) ->
    (0 `lt` length) ==>
    forAllShrink (choose (0, fromTime length)) shrink $ \ (Time -> time) ->
    (0 `lt` time && time `lt` length) ==>
    forAllShrink (listOf1 (choose (1, 10))) shrink $ \ (intWeights :: [Int]) ->
    all (> 0) intWeights ==>
    let weights = map fromIntegral intWeights
    in \ testSignals ->
    let parts = map (\ (weight, testSignal) -> weight ~> \ _time -> toSignal testSignal) $ Prelude.zip weights testSignals
    in counterexample (show $ Prelude.zip weights testSignals) $ do
      signalLength (divide parts length) `shouldBeCloseTo`
        signalLength (simpleDivide parts length)
      getSample (divide parts length) time `shouldBeCloseTo`
        getSample (simpleDivide parts length) time
