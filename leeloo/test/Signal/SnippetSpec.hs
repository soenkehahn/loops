module Signal.SnippetSpec where

import Prelude ()
import Signal
import Signal.Snippet
import Test.Hspec
import Test.Utils

spec :: Spec
spec = do
  describe "snippets" $ do
    it "can be constructed from a signal" $ do
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
