module Signal.UtilsSpec where

import Test.Hspec
import Signal.Utils
import Signal
import Test.Utils

spec :: Spec
spec = do
  describe "fanOut" $ do
    it "applies the given function to every element and adds the results" $ do
      let signal = fanOut (\ x -> constant x) [1.0, 0.2, 0.03]
      test 1 1 signal [1.23 :: Double]
