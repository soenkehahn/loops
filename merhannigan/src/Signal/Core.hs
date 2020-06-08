{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Signal.Core (
  Time(..),
  minTime,
  maxTime,
  maximumTime,
  Length(..),
  mapLength,
  minLength,
  maxLength,
  Signal(..),
  simpleSignal,
  getSample,
  runOnTimes,
  Signal.Core.toList,
  toVector,
  printSamples,

  -- exported for testing
  _getSampleTimes,
  _equalSlices,
) where

import Signal.Epsilon
import System.IO
import Data.ByteString.Conversion
import Control.Monad.ST
import Data.Traversable
import Data.Foldable
import qualified Data.ByteString.Char8 as BS
import Data.Vector.Storable (Storable, Vector, generate, slice)
import qualified Data.Vector.Storable as Vec

newtype Time = Time {
  fromTime :: Double
} deriving (Num, Fractional, Enum, Storable)

instance Show Time where
  show (Time t) = show t

instance EpsilonOrd Time where
  compare (Time a) (Time b) = Signal.Epsilon.compare a b

minTime :: Time -> Time -> Time
minTime a b = Time $ min (fromTime a) (fromTime b)

maxTime :: Time -> Time -> Time
maxTime a b = Time $ max (fromTime a) (fromTime b)

maximumTime :: [Time] -> Time
maximumTime = Time . maximum . map fromTime

data Length
  = Finite Time
  | Infinite
  deriving Show

instance EpsilonEq Length where
  Infinite ==== Infinite = True
  Finite a ==== Finite b = a ==== b
  _ ==== _ = False

mapLength :: (Time -> Time) -> Length -> Length
mapLength f length = case length of
  Finite time -> Finite (f time)
  Infinite -> Infinite

minLength :: Length -> Length -> Length
minLength a b = case (a, b) of
  (Finite a, Finite b) -> Finite $ minTime a b
  (Finite a, Infinite) -> Finite a
  (Infinite, Finite b) -> Finite b
  (Infinite, Infinite) -> Infinite

maxLength :: Length -> Length -> Length
maxLength a b = case (a, b) of
  (Finite a, Finite b) -> Finite $ maxTime a b
  (_, Infinite) -> Infinite
  (Infinite, _) -> Infinite

data Signal a = Signal {
  end :: Length,
  initialize :: forall s . ST s (Time -> ST s a)
} deriving (Functor)

instance Applicative Signal where
  pure a = Signal Infinite $ return $ \ _time -> return a
  fSignal <*> xSignal =
    Signal (minLength (end fSignal) (end xSignal)) $ do
      runF <- initialize fSignal
      runX <- initialize xSignal
      return $ \ time -> do
        runF time <*> runX time

getSample :: Signal a -> Time -> a
getSample signal time = runST $ do
  runSignal <- initialize signal
  runSignal time

_getSampleTimes :: Time -> Time -> Vector Time
_getSampleTimes delta end = case end of
  end -> generate (epsilonCeiling (end / delta)) $ \ i ->
    fromIntegral i * delta
  where
    epsilonCeiling :: Time -> Int
    epsilonCeiling x =
      let c = ceiling $ fromTime x
      in if Time (realToFrac (c - 1)) ==== x then c - 1 else c

runOnTimes :: Storable a => Signal a -> Vector Time -> Vector a
runOnTimes signal sampleTimes = runST $ do
    runSignal <- initialize signal
    Vec.mapM runSignal sampleTimes

toList :: Time -> Signal a -> [a]
toList delta signal = case end signal of
  Infinite -> error "toList: infinite signals not supported"
  Finite end -> runST $ do
    runSignal <- initialize signal
    forM (Vec.toList (_getSampleTimes delta end)) $ \ time ->
      runSignal time

toVector :: Storable a => Time -> Signal a -> Vector a
toVector delta signal = case end signal of
  Infinite -> error "toVector: infinite signals not supported"
  Finite end -> runOnTimes signal $ _getSampleTimes delta end

_equalSlices :: Storable a => Int -> Vector a -> [Vector a]
_equalSlices n vector =
  flip map [0 .. n - 1] $ \ i ->
    let start = round $ position i
        end = round $ position (i + 1)
    in slice
      start
      (min (end - start) (Vec.length vector - start))
      vector
  where
    position :: Int -> Double
    position i = realToFrac (i * Vec.length vector) / realToFrac n

printSamples :: Signal Double -> IO ()
printSamples signal = case end signal of
  Infinite -> error "printSamples: infinite signals not supported"
  Finite end -> do
    hPutStrLn stderr ("end: " ++ show end)
    let sampleTimesChunks =
          _equalSlices 4 $
          _getSampleTimes (1 / 44100) end
        chunks = runST $ do
          runSignal <- initialize signal
          forM sampleTimesChunks $ \ sampleTimes -> do
            Vec.forM sampleTimes $ \ time -> do
              sample <- runSignal time
              return sample
    forM_ chunks $ \ chunk -> do
      Vec.forM_ chunk $ \ sample -> do
        BS.putStrLn $ toByteString' sample

simpleSignal :: (Time -> a) -> Signal a
simpleSignal function = Signal Infinite $ do
  return $ \ time -> do
    return $ function time
