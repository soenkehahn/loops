module SnippetSpec where

import Snippet
import Signal
import Test.Utils
import Test.Hspec
import Prelude ()

spec :: Spec
spec = do
  describe "snippets" $ do
    it "can be constructed from a signal" $ do
      let snippet = 0 |-> constant 42
      test 1 2 (runSnippet snippet) [42, 42]

    it "will delay the given signal by the given time" $ do
      let snippet = 1 |-> constant 42
      test 1 2 (runSnippet snippet) [0, 42]

    it "allows to add snippets together" $ do
      let snippet =
            1 |-> take 1 (constant 23) <>
            2 |-> take 1 (constant 42)
      test 1 3 (runSnippet snippet) [0, 23, 42]

    it "allows overlapping snippets" $ do
      let snippet =
            1 |-> take 2 (constant 23) <>
            2 |-> take 2 (constant 42)
      test 1 5 (runSnippet snippet) [0, 23, 65, 42]

    it "has good fixity in conjunction with |> and /\\" $ do
      let s = constant 0
          _snippet =
            0 |-> s |> s <>
            1 |-> s /\ s <>
            mempty
      return () :: IO ()
