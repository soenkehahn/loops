module Signal.Utils where

import Signal
import Data.List

fanOut :: Num b => [a] -> (a -> Signal b) -> Signal b
fanOut list signal =
    foldl' (\ acc a -> acc +++ signal a) empty list

harmonics :: [Double] -> Signal Double
harmonics weights =
  foldl' (\ acc (natural, weight) -> acc +++ tone natural weight) empty $
    Prelude.zip [1 ..] weights
  where
    tone natural weight =
      fmap (* weight) (constSpeedup natural sine)
