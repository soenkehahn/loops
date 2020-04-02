module Signal.Snippet where

import Signal

infix 7 |->
(|->) :: Num a => Time -> Signal a -> Signal a
time |-> signal = silence time |> signal
