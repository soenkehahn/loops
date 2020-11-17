module Signal.Utils where

import Data.List (foldl')
import Signal
import Signal.Core
import Signal.Memoize
import Prelude (zip)

fanOut :: Num b => [a] -> (a -> Signal b) -> Signal b
fanOut list signal =
  foldl' (\acc a -> acc +++ signal a) empty list

harmonics :: [Double] -> Signal Double
harmonics weights =
  memoizeWave $
    unmemoizedHarmonics weights

unmemoizedHarmonics :: [Double] -> Signal Double
unmemoizedHarmonics weights = do
  foldl' (\acc (natural, weight) -> acc +++ tone natural weight) empty $
    Prelude.zip [1 ..] weights
  where
    tone natural weight =
      fmap (* weight) (constSpeedup natural sine)
