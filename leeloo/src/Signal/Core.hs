{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Signal.Core where

import Signal.Epsilon
import System.IO
import Data.ByteString.Conversion
import Control.Monad.ST
import qualified Data.ByteString.Char8 as BS

newtype Time = Time {
  fromTime :: Double
} deriving (Num, Fractional, Enum)

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

minLength :: Signal a -> Signal b -> Length
minLength a b = case (signalLength a, signalLength b) of
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
  signalLength :: Length,
  initialize :: forall s . ST s (Time -> ST s a)
} deriving (Functor)

instance Applicative Signal where
  pure a = Signal Infinite $ return $ \ _time -> return a
  fSignal <*> xSignal =
    Signal (minLength fSignal xSignal) $ do
      runF <- initialize fSignal
      runX <- initialize xSignal
      return $ \ time -> do
        runF time <*> runX time

getSample :: Signal a -> Time -> a
getSample signal time = runST $ do
  runSignal <- initialize signal
  runSignal time

deltas :: Time -> Length -> [Time]
deltas delta end = case end of
  Finite end -> takeWhile (`lt` end) [0, delta ..]
  Infinite -> [0, delta ..]

runOnDeltas :: Signal a -> [Time] -> [a]
runOnDeltas signal deltas = runST $ do
    runSignal <- initialize signal
    mapM runSignal deltas

toList :: Time -> Signal a -> [a]
toList delta signal = runOnDeltas signal $ deltas delta (signalLength signal)

foreach :: Signal a -> [Time] -> (a -> IO ()) -> IO ()
foreach signal deltas action = mapM_ action $ runOnDeltas signal deltas

printSamples :: Signal Double -> IO ()
printSamples signal = do
  case signalLength signal of
    Infinite -> error "infinite signal"
    Finite end -> do
      hPutStrLn stderr ("length: " ++ show end)
      foreach signal (deltas (1 / 44100) (Finite end)) $ \ sample -> do
        BS.putStrLn $ toByteString' sample

simpleSignal :: (Time -> a) -> Signal a
simpleSignal function = Signal Infinite $ do
  return $ \ time -> do
    return $ function time
