module Signal.Memoize where

import Control.Monad.ST
import Data.Vector.Storable (Vector, generateM, (!))
import Signal
import Signal.Epsilon

memoize :: Int -> Signal Double -> Signal Double
memoize arrayLength signal = case signalLength signal of
  Finite length | length ==== 0 -> empty
  Finite length ->
    let array :: Vector Double
        array = runST $ do
          runSignal <- initialize signal
          generateM arrayLength $ \ index -> do
            runSignal (_toTime length arrayLength index)
    in Signal (signalLength signal) $ do
      return $ \ time -> do
        return $ array ! (_toIndex length arrayLength time)
  Infinite -> error "memoize not implemented for infinite signals"

_toIndex :: Time -> Int -> Time -> Int
_toIndex length arrayLength time =
  floor (fromTime (fromIntegral arrayLength * time / length))

_toTime :: Time -> Int -> Int -> Time
_toTime length arrayLength index =
  length * fromIntegral index / fromIntegral arrayLength
