module Test.Utils where

import Prelude ()
import Signal
import Test.Hspec
import Test.HUnit (assertFailure)
import Signal.Epsilon
import Control.Monad.ST

getSample :: Signal a -> Time -> a
getSample signal time = runST $ do
  runSignal <- initialize signal
  runSignal time

test :: (HasCallStack, EpsilonEq a, Show a) => Time -> Time -> Signal a -> [a] -> IO ()
test delta length signal expected =
  toList delta (take length signal) `shouldBeCloseTo` expected

shouldBeCloseTo :: (HasCallStack, EpsilonEq a, Show a) => a -> a -> IO ()
shouldBeCloseTo got expected =
  if got ==== expected
    then return ()
    else assertFailure $
      "expected: " ++ show expected ++ "\n but got: " ++ show got
