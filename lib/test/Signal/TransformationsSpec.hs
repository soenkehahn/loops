module Signal.TransformationsSpec where

import Signal
import Signal.Transformations
import Test.Hspec
import Test.Utils

spec :: Spec
spec = do
  describe "echo" $ do
    it "produces a single echo" $ do
      let signal =
            echo
              0.2
              0.5
              (ramp 0 1 1)
          expected =
            Prelude.zipWith
              (+)
              [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.0, 0.0]
              [0.0, 0.0, 0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45]
      test 0.1 10 signal expected
