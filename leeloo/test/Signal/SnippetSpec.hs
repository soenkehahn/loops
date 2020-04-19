{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Signal.SnippetSpec where

import Control.Monad
import Data.Fixed
import Prelude ()
import Control.Monad.ST
import Signal
import Signal.Snippet
import Signal.Epsilon
import Test.Hspec
import Test.Utils
import qualified Data.Vector as Vec
import Test.QuickCheck
import Data.Vector ((!))
import qualified Prelude

spec :: Spec
spec = describe "" $ do
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
        n value length = Signal (Just length) $ do
          return $ \ time -> return $ if time `lt` length then value else 0

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

      it "doesn't ever make signals overlap that shouldn't overlap" $ property $
        forAllShrink (choose (0, 100)) shrink $ \ (Time -> length) ->
        (0 `lt` length) ==>
        forAllShrink (choose (0, fromTime length)) shrink $ \ (Time -> time) ->
        (0 `lt` time && time `lt` length) ==>
        forAllShrink (listOf1 (choose (1, 10))) shrink $ \ (intWeights :: [Int]) ->
        let weights = map fromIntegral intWeights
        in (not (null weights) && all (> 0) weights) ==> do
          let signal = divide (map (\ weight -> weight ~> n 1) weights) length
              sample = runST $ do
                runSignal <- initialize signal
                runSignal time
          sample `shouldBe` 1

      it "can handle time inputs that are less than epsilon below 0" $ do
        let signal = divide [1 ~> n 1] 1
            sample = runST $ do
              runSignal <- initialize signal
              runSignal $ Time (0 - (epsilon / 20))
        sample `shouldBe` 1

      it "can handle infinite signals" $ do
        pending

    describe "evenly" $ do
      it "divides evenly" $ do
        let signal = evenly [n 1, n 2, n 3, n 4] 8
        test 1 10 signal [1, 1, 2, 2, 3, 3, 4, 4]

    describe "raster" $ do
      it "uses the given length as the time unit" $ do
        let signal = raster 2 [1 ~> n 1, 1 ~> n 2]
        test 1 10 signal [1, 1, 2, 2]

  describe "mkSnippets_" $ do
    let constSignal = \ len -> take len (constant (42 :: Double))

    it "returns snippets with start times, starting at 0" $ do
      let parts = [Part 1 (\ len -> take len (constant 42))]
      map fst (mkSnippets_ parts 1) `shouldBeCloseTo` [0]

    it "consecutive snippets are being started later by one time-unit" $ do
      let parts = [Part 1 constSignal, Part 1 constSignal]
      map fst (mkSnippets_ parts 2) `shouldBeCloseTo` [0, 1]

    it "takes the weights into account" $ do
      let parts = [Part 0.5 constSignal, Part 1.5 constSignal]
      map fst (mkSnippets_ parts 2) `shouldBeCloseTo` [0, 0.5]

    it "takes the overall length into account" $ do
      let parts = [Part 1 constSignal, Part 1 constSignal]
      map fst (mkSnippets_ parts 3) `shouldBeCloseTo` [0, 1.5]

    it "passes the length of the part to the signal" $ do
      let parts = [Part 4 constSignal, Part 2 constSignal]
          signal = snd $ head $ mkSnippets_ parts 3
      test 1 5 signal [42, 42]

  describe "mkVector_" $ do
    let withEnd :: Time -> (Time -> Double) -> Signal Double
        withEnd end f = Signal (Just end) $ return $ \ time -> return (f time)

        runMkVector_ :: Int -> [(Time, Signal Double)] -> [Double]
        runMkVector_ vectorLength snippets = runST $ do
          (_, vector) <- mkVector_ vectorLength snippets
          samples <- forM (Prelude.zip [0 ..] (Vec.toList vector)) $ \ (i, cell) ->
            cell (i * 1)
          return $ samples

    it "returns a singleton vector with the given signal in it" $ do
      let samples = runMkVector_ 1 [(0, withEnd 1 (const 42))]
      samples `shouldBe` [42]

    it "puts a single signal into two cells when the signal overlaps both cells" $ do
      let samples = runMkVector_ 2 [(0, withEnd 2 (const 42))]
      samples `shouldBe` [42, 42]

    it "adds two signals when they overlap" $ do
      let samples = runMkVector_ 1 [(0, withEnd 1 (const 3)), (0, withEnd 1 (const 4))]
      samples `shouldBe` [7]

    it "doesn't put the signal in a cell that ends before the signal starts" $ do
      let samples = runMkVector_ 2 [(0, withEnd 2 (const 3)), (1, withEnd 1 (const 4))]
      samples `shouldBe` [3, 7]

    it "doesn't put the signal in a cell that starts after the signal ends" $ do
      let samples = runMkVector_ 2 [(0, withEnd 2 (const 3)), (0, withEnd 1 (const 4))]
      samples `shouldBe` [7, 3]

    it "shifts the signals by their start date" $ do
      let samples = runMkVector_ 4 [(0, withEnd 4 (const 1)), (2, ramp 1 2 2)]
      samples `shouldBe` [1, 1, 2, 2.5]

  describe "_vectorLength" $ do
    it "does stuff" $ do
      pending

  describe "computeIndex" $ do
    it "returns an index pointing to the first cell for time 0" $ do
      computeIndex_ 10 10 0 `shouldBe` 0

    it "returns an index pointing to a later cell for later times" $ do
      computeIndex_ 10 10 5 `shouldBe` 5

    it "works for times in the middle of a cell" $ do
      computeIndex_ 10 10 5.5 `shouldBe` 5

    it "works for times that are less than epsilon below 0" $ do
      computeIndex_ 10 10 (0 - Time epsilon / 2) `shouldBe` 0

    it "returns the next cell when the time is less than epsilon below the next cell" $ do
      computeIndex_ 10 10 (5 - Time epsilon / 2) `shouldBe` 5

  bla

data TestSignal
  = TakeConstant Double Double
  deriving (Show)

toSignal :: TestSignal -> Signal Double
toSignal = \case
  TakeConstant length value ->
    take (Time length) $
    take (Time length) (constant value) |> constant 0

instance Arbitrary TestSignal where
  arbitrary = oneof [
    TakeConstant <$> choose (0, 10) <*> choose (-1, 1)
    ]
  shrink = \case
    TakeConstant length value ->
      (TakeConstant <$> shrink length <*> pure value) ++
      (TakeConstant <$> pure length <*> shrink value)

getSample :: Signal a -> Time -> a
getSample signal time = runST $ do
  runSignal <- initialize signal
  runSignal time

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

bla :: Spec
bla = describe "property tests" $ do
  it "foo" $ do
    let parts = [
            1 .> take 1 (constant 0.1),
            1 .> take 3 (constant 0)
          ]

    print $ getSample (divide parts 1) 0.4
    getSample (divide parts 1) 0.3 `shouldBe` (0.1 :: Double)

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
    in counterexample (show $ Prelude.zip weights testSignals) $
      getSample (divide parts length) time `shouldBe` getSample (simpleDivide parts length) time
