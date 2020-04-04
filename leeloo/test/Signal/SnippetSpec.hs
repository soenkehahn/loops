module Signal.SnippetSpec where

import Prelude ()
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
