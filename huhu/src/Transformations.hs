module Transformations where

import Signal

echo :: Time -> Double -> Signal Double -> Signal Double
echo delay volume signal =
  signal +++
  fmap (* volume) (silence delay |> signal)
