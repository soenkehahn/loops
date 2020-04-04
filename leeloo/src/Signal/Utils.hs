module Signal.Utils where

import Signal
import Data.List

fanOut :: Num b => (a -> Signal b) -> [a] -> Signal b
fanOut signal =
    foldl' (\ acc a -> acc +++ signal a) empty

harmonics :: [Double] -> Signal Double
harmonics weights =
  foldl' (\ acc (natural, weight) -> acc +++ tone natural weight) empty $
    Prelude.zip [1 ..] weights
  where
    tone natural weight =
      fmap (* weight) (constSpeedup natural sine)
