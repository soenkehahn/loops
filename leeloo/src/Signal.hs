{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Signal (
  module Prelude,
  module Signal,
) where

import GHC.Stack
import Control.Exception
import Control.Monad.ST
import Data.ByteString.Conversion
import Data.Fixed
import Data.Function
import Data.List (foldl')
import Data.STRef
import Data.Vector ((!))
import Signal.Epsilon
import Prelude hiding (take, repeat, cycle, zip, zipWith)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as VS
import System.IO
import System.Random

-- signal basics

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

data Signal a = Signal {
  signalLength :: Length,
  initialize :: forall s . ST s (Time -> ST s a)
} deriving (Functor)

getSample :: Signal a -> Time -> a
getSample signal time = runST $ do
  runSignal <- initialize signal
  runSignal time

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

instance Applicative Signal where
  pure = constant
  fSignal <*> xSignal =
    Signal (minLength fSignal xSignal) $ do
      runF <- initialize fSignal
      runX <- initialize xSignal
      return $ \ time -> do
        runF time <*> runX time

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

constant :: a -> Signal a
constant a = Signal Infinite $ return $ \ _time -> return a

take :: Signal a -> Time -> Signal a
take (Signal mEnd signal) length = Signal end' signal
  where
    end' = Finite $ case mEnd of
      Infinite -> length
      Finite end -> minTime end length

skip :: Time -> Signal a -> Signal a
skip length signal =
  Signal (mapLength (\ end -> maxTime 0 (end - length)) (signalLength signal)) $ do
    runSignal <- initialize signal
    return $ \ time -> do
      runSignal (time + length)

focus :: Time -> Time -> Signal a -> Signal a
focus start length signal = take (skip start signal) length

fromList :: Time -> [a] -> Signal a
fromList delta (Vec.fromList -> vec) =
  Signal (Finite $ fromIntegral (length vec) * delta) $ do
    return $ \ time -> do
      return $ vec ! floor (fromTime time / fromTime delta)

random :: Random a => (a, a) -> Signal a
random bounds =
  Signal Infinite $ do
    genRef <- newSTRef (mkStdGen 42)
    return $ \ _time -> do
      gen <- readSTRef genRef
      let (sample, newGen) = randomR bounds gen
      writeSTRef genRef newGen
      return sample

empty :: Signal a
empty = Signal (Finite 0) $ return $ \ time ->
  error ("empty: shouldn't be called. (" ++ show time ++ ")")

zip :: Signal a -> Signal b -> Signal (a, b)
zip a b = Signal (minLength a b) $ do
  runA <- initialize a
  runB <- initialize b
  return $ \ time -> do
    a <- runA time
    b <- runB time
    return (a, b)

zipWith :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipWith f a b = Signal (minLength a b) $ do
  runA <- initialize a
  runB <- initialize b
  return $ \ time -> do
    a <- runA time
    b <- runB time
    return $ f a b

simpleSignal :: (Time -> a) -> Signal a
simpleSignal function = Signal Infinite $ do
  return $ \ time -> do
    return $ function time

-- audio signals

tau :: Double
tau = pi * 2

phase :: Signal Double
phase = Signal Infinite $ return $ \ time ->
  return $ phase_array VS.! (round (fromTime time * 44100) `mod` 44100)

phase_array :: VS.Vector Double
phase_array = VS.generate 44100 $ \ index ->
  tau * fromIntegral index / 44100

project :: (Double, Double) -> (Double, Double) -> Double -> Double
project (fromLow, fromHigh) (toLow, toHigh) x =
  (((x - fromLow) / (fromHigh - fromLow)) * (toHigh - toLow)) + toLow

clip :: (Double, Double) -> Double -> Double
clip (lower, upper) x = max lower (min upper x)

speedup :: Signal Double -> Signal a -> Signal a
speedup factorSignal inputSignal = case signalLength inputSignal of
  Infinite -> Signal (signalLength factorSignal) $ do
    runFactorSignal <- initialize $ integral factorSignal
    runInputSignal <- initialize inputSignal
    return $ \ time -> do
      integrated <- runFactorSignal time
      runInputSignal $ Time integrated
  Finite _ -> error "speedup not supported for finite input signals"

integral :: Signal Double -> Signal Double
integral signal = Signal (signalLength signal) $ do
  ref <- newSTRef 0
  lastTimeRef <- newSTRef 0
  runSignal <- initialize signal
  return $ \ time -> do
    lastTime <- readSTRef lastTimeRef
    writeSTRef lastTimeRef time
    current <- readSTRef ref
    factor <- runSignal time
    let next = current + (fromTime time - fromTime lastTime) * factor
    writeSTRef ref next
    return next

constSpeedup :: Double -> Signal a -> Signal a
constSpeedup (Time -> factor) signal =
  Signal (mapLength (/ factor) (signalLength signal)) $ do
    runSignal <- initialize signal
    return $ \ time -> do
      runSignal (time * factor)

(|>) :: Signal a -> Signal a -> Signal a
a |> b = case signalLength a of
  Infinite -> a
  Finite aEnd ->
    Signal (mapLength (+ aEnd) (signalLength b)) $ do
      runA <- initialize a
      runB <- initialize b
      return $ \ time -> do
        if time `lt` aEnd
          then runA time
          else runB (time - aEnd)

repeat :: Integer -> Signal a -> Signal a
repeat n signal =
  if n <= 0
    then empty
    else signal |> repeat (n - 1) signal

cycle :: Signal a -> Signal a
cycle signal = case signalLength signal of
  Infinite -> signal
  Finite end -> Signal Infinite $ do
    runSignal <- initialize signal
    return $ \ time ->
      runSignal $ Time (fromTime time `mod'` fromTime end)

infixr 6 +++
(+++) :: Num a => Signal a -> Signal a -> Signal a
a +++ b =
  let isValidTime :: Signal a -> Time -> Bool
      isValidTime signal time = case signalLength signal of
        Infinite -> True
        Finite end -> time `lt` end
  in Signal (maxLength (signalLength a) (signalLength b)) $ do
    runA <- initialize a
    runB <- initialize b
    return $ \ time -> do
      x <- if isValidTime a time
        then runA time
        else return 0
      y <- if isValidTime b time
        then runB time
        else return 0
      return (x + y)

mix :: Num a => [Signal a] -> Signal a
mix = foldl' (+++) empty

(/\) :: Num a => Signal a -> Signal a -> Signal a
a /\ b = (*) <$> a <*> b

silence :: Num a => Time -> Signal a
silence length = take (constant 0) length

fill :: Num a => Time -> Signal a -> Signal a
fill length signal = take (signal |> constant 0) length

saw :: Signal Double
saw = fmap (project (0, tau) (-1, 1)) phase

sine :: Signal Double
sine = fmap sin phase

rect :: Signal Double
rect = fmap (\ x -> if x < tau / 2 then -1 else 1) phase

ramp :: Double -> Double -> Time -> Signal Double
ramp _ _ length | length ==== 0 = empty
ramp start end length = Signal (Finite length) $ return $ \ time ->
  return $ (fromTime time / fromTime length) * (end - start) + start

data Adsr = Adsr {
  attack :: Time,
  decay :: Time,
  sustain :: Double,
  release :: Time
}

instance Show Adsr where
  show (Adsr a d s r) =
    "(Adsr " ++ show a ++ " " ++ show d ++ " " ++ show s ++ " " ++ show r ++ ")"

adsr :: HasCallStack => Time -> Adsr -> Signal Double
adsr length config@(Adsr attack decay sustain release) =
  if length `lt` (attack + decay)
    then error $
      show config ++ " requires a length longer than " ++
      show (attack + decay) ++ ", given length: " ++
      show length
    else envelope
  where
    envelope =
      ramp 0 1 attack |>
      ramp 1 sustain decay |>
      take (constant sustain) (length - attack - decay) |>
      ramp sustain 0 release
