module Transformations where

import Signal

echo :: Double -> Double -> Signal Double -> Signal Double
echo delay volume signal =
  signal +++
  fmap (* volume) (shift delay signal)
