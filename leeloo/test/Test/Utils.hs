module Test.Utils where

import Prelude ()
import Signal
import Signal.Core
import Test.Hspec
import Test.HUnit (assertFailure)
import Signal.Epsilon

shouldYield :: (Show a, EpsilonEq a) => Signal a -> [a] -> IO ()
shouldYield signal expected = do
  test 0.5 1.5 signal expected

test :: (HasCallStack, EpsilonEq a, Show a) => Time -> Time -> Signal a -> [a] -> IO ()
test delta length signal expected =
  toList delta (take signal length) `shouldBeCloseTo` expected

shouldBeCloseTo :: (HasCallStack, EpsilonEq a, Show a) => a -> a -> IO ()
shouldBeCloseTo got expected =
  if got ==== expected
    then return ()
    else assertFailure $
      "expected: " ++ show expected ++ "\n but got: " ++ show got
