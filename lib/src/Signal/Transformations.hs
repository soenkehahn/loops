module Signal.Transformations where

import Signal
import Signal.Core

echo :: Time -> Double -> Signal Double -> Signal Double
echo delay volume signal =
  signal
    +++ fmap (* volume) (silence delay |> signal)
