module Test.Utils where

import Prelude ()
import Signal
import Test.Hspec
import Test.HUnit (assertFailure)

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
