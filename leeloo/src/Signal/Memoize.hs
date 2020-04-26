{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Memoize where

import Control.Monad.ST
import Data.Vector.Storable (Vector, generateM, (!), Storable)
import Signal
import Signal.Epsilon
import Prelude ()

memoize :: forall a . Storable a => Int -> Signal a -> Signal a
memoize arrayLength signal = case signalLength signal of
  Finite length | length ==== 0 -> empty
  Finite length ->
    let array :: Vector a
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

mem :: Storable a => Signal a -> Signal a
mem signal = case signalLength signal of
  Finite length -> memoize (round $ fromTime (length * 44100)) signal
  Infinite -> error "memoize not implemented for infinite signals"
