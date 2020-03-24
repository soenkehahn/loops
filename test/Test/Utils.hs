module Test.Utils where

import Prelude ()
import Signal
import Test.Hspec
import Test.HUnit (assertFailure)
import qualified Data.Vector as Vec
import Epsilon

test :: (HasCallStack, EpsilonEq a, Show a) => Time -> Time -> Signal a -> [a] -> IO ()
test delta length signal expected =
  toVector delta (take length signal) `shouldBeCloseTo` Vec.fromList expected

shouldBeCloseTo :: (HasCallStack, EpsilonEq a, Show a) => a -> a -> IO ()
shouldBeCloseTo got expected =
  if got ==== expected
    then return ()
    else assertFailure $
      "expected: " ++ show expected ++ "\n but got: " ++ show got
