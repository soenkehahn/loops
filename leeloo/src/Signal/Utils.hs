module Signal.Utils where

import Signal
import Data.List

fanOut :: Num b => (a -> Signal b) -> [a] -> Signal b
fanOut signal =
    foldl' (\ acc a -> acc +++ signal a) empty
