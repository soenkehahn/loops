{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Signal (
  module Prelude,
  module Signal,
) where

import Control.Exception
import Control.Monad.ST
import Data.ByteString.Conversion
import Data.Fixed
import Data.Function
import Data.List (foldl')
import Data.STRef
import Data.Vector (Vector, (!), (!?))
import Epsilon
import Prelude hiding (take, repeat, cycle, zip, zipWith)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as Vec
import System.IO
import System.Random

-- signal basics

newtype Time = Time {
  fromTime :: Double
} deriving (Num, Fractional, Enum)

instance Show Time where
  show (Time t) = show t

instance EpsilonOrd Time where
  compare (Time a) (Time b) = Epsilon.compare a b

minTime :: Time -> Time -> Time
minTime a b = Time $ min (fromTime a) (fromTime b)

maxTime :: Time -> Time -> Time
maxTime a b = Time $ max (fromTime a) (fromTime b)

data Signal a = Signal {
  end :: Maybe Time,
  initialize :: forall s . ST s (Time -> ST s a)
} deriving (Functor)

minEnd :: Signal a -> Signal b -> Maybe Time
minEnd a b = case (end a, end b) of
  (Just a, Just b) -> Just $ minTime a b
  (Just a, Nothing) -> Just a
  (Nothing, Just b) -> Just b
  (Nothing, Nothing) -> Nothing

maxEnd :: Signal a -> Signal b -> Maybe Time
maxEnd a b = case (end a, end b) of
  (Just a, Just b) -> Just $ maxTime a b
  (_, Nothing) -> Nothing
  (Nothing, _) -> Nothing

instance Applicative Signal where
  pure = constant
  fSignal <*> xSignal =
    Signal (minEnd fSignal xSignal) $ do
      runF <- initialize fSignal
      runX <- initialize xSignal
      return $ \ time -> do
        runF time <*> runX time

deltas :: Time -> Maybe Time -> [Time]
deltas delta end = case end of
  Just end -> takeWhile (`lt` end) [0, delta ..]
  Nothing -> [0, delta ..]

runOnDeltas :: Signal a -> [Time] -> [a]
runOnDeltas signal deltas = runST $ do
    runSignal <- initialize signal
    mapM runSignal deltas

toList :: Time -> Signal a -> [a]
toList delta signal = runOnDeltas signal $ deltas delta (end signal)

foreach :: Signal a -> [Time] -> (a -> IO ()) -> IO ()
foreach signal deltas action = mapM_ action $ runOnDeltas signal deltas

printSamples :: Signal Double -> IO ()
printSamples signal = do
  case (end signal) of
    Nothing -> error "infinite signal"
    Just end -> do
      hPutStrLn stderr ("length: " ++ show end)
      foreach signal (deltas (1 / 44100) (Just end)) $ \ sample -> do
        BS.putStrLn $ toByteString' sample

constant :: a -> Signal a
constant a = Signal Nothing $ return $ \ _time -> return a

take :: Time -> Signal a -> Signal a
take length (Signal mEnd signal) = Signal end' signal
  where
    end' = Just $ case mEnd of
      Nothing -> length
      Just end -> minTime end length

skip :: Time -> Signal a -> Signal a
skip length signal =
  Signal (fmap (\ end -> maxTime 0 (end - length)) (end signal)) $ do
    runSignal <- initialize signal
    return $ \ time -> do
      runSignal (time + length)

focus :: Time -> Time -> Signal a -> Signal a
focus start length signal = take length $ skip start signal

fromList :: Time -> [a] -> Signal a
fromList delta (Vec.fromList -> vec) =
  Signal
    (Just $ fromIntegral (length vec) * delta)
    (return $ \ time -> return $ vec ! floor (fromTime time / fromTime delta))

random :: Random a => (a, a) -> Signal a
random bounds =
  Signal Nothing $ do
    genRef <- newSTRef (mkStdGen 42)
    return $ \ _time -> do
      gen <- readSTRef genRef
      let (sample, newGen) = randomR bounds gen
      writeSTRef genRef newGen
      return sample

empty :: Signal a
empty = Signal (Just 0) $ return $ \ time ->
  error ("empty: shouldn't be called. (" ++ show time ++ ")")

zip :: Signal a -> Signal b -> Signal (a, b)
zip a b = Signal (minEnd a b) $ do
  runA <- initialize a
  runB <- initialize b
  return $ \ time -> do
    a <- runA time
    b <- runB time
    return (a, b)

zipWith :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipWith f a b = Signal (minEnd a b) $ do
  runA <- initialize a
  runB <- initialize b
  return $ \ time -> do
    a <- runA time
    b <- runB time
    return $ f a b

-- audio signals

tau :: Double
tau = pi * 2

phase :: Signal Double
phase = Signal Nothing $ return $ \ time ->
  return $ (fromTime time `mod'` 1) * tau

project :: (Double, Double) -> (Double, Double) -> Double -> Double
project (fromLow, fromHigh) (toLow, toHigh) x =
  (((x - fromLow) / (fromHigh - fromLow)) * (toHigh - toLow)) + toLow

clip :: (Double, Double) -> Double -> Double
clip (lower, upper) x = max lower (min upper x)

speedup :: Signal Double -> Signal a -> Signal a
speedup factorSignal inputSignal = case end inputSignal of
  Nothing -> Signal (end factorSignal) $ do
    runFactorSignal <- initialize $ integral factorSignal
    runInputSignal <- initialize inputSignal
    return $ \ time -> do
      integrated <- runFactorSignal time
      runInputSignal $ Time integrated
  Just _ -> error "speedup not supported for finite input signals"

integral :: Signal Double -> Signal Double
integral signal = Signal (end signal) $ do
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
  Signal (fmap (/ factor) (end signal)) $ do
    runSignal <- initialize signal
    return $ \ time -> do
      runSignal (time * factor)

(|>) :: Signal a -> Signal a -> Signal a
a |> b = case end a of
  Nothing -> a
  Just aEnd ->
    Signal (fmap (+ aEnd) (end b)) $ do
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
cycle signal = case end signal of
  Nothing -> signal
  Just end -> Signal Nothing $ do
    runSignal <- initialize signal
    return $ \ time ->
      runSignal $ Time (fromTime time `mod'` fromTime end)

(+++) :: Num a => Signal a -> Signal a -> Signal a
a +++ b =
  let isValidTime :: Signal a -> Time -> Bool
      isValidTime signal time = case end signal of
        Nothing -> True
        Just end -> time `lt` end
  in Signal (maxEnd a b) $ do
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

zipWithOverlapping :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
zipWithOverlapping f as bs =
  Vec.generate (max (Vec.length as) (Vec.length bs)) $ \ i ->
    case (as !? i, bs !? i) of
      (Just a, Just b) -> f a b
      (Just a, Nothing) -> a
      (Nothing, Just b) -> b
      (Nothing, Nothing) -> error "zipWithOverlapping: shouldn't happen"

(/\) :: Num a => Signal a -> Signal a -> Signal a
a /\ b = (*) <$> a <*> b

silence :: Num a => Time -> Signal a
silence length = take length (constant 0)

fill :: Num a => Time -> Signal a -> Signal a
fill length signal = take length (signal |> constant 0)

saw :: Signal Double
saw = fmap (project (0, tau) (-1, 1)) phase

sine :: Signal Double
sine = fmap sin phase

rect :: Signal Double
rect = fmap (\ x -> if x < tau / 2 then -1 else 1) phase

ramp :: Time -> Double -> Double -> Signal Double
ramp length _ _ | length ==== 0 = empty
ramp length start end = Signal (Just length) $ return $ \ time ->
  return $ (fromTime time / fromTime length) * (end - start) + start

data Adsr = Adsr {
  attack :: Time,
  decay :: Time,
  sustain :: Double,
  release :: Time
}

adsr :: Time -> Adsr -> Signal Double -> Signal Double
adsr length (Adsr attack decay sustain release) signal =
  envelope /\ signal
  where
    envelope =
      ramp attack 0 1 |>
      ramp decay 1 sustain |>
      take (length - attack - decay - release) (constant sustain) |>
      ramp release sustain 0
