{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Signal (
  module Prelude,
  module Signal,
) where

import GHC.Stack
import Control.Exception
import Signal.Core
import Data.Fixed
import Data.Function
import Data.List (foldl')
import Data.STRef
import Data.Vector ((!))
import Signal.Epsilon
import Prelude hiding (take, repeat, cycle, zip, zipWith)
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as VS
import System.Random

constant :: a -> Signal a
constant = pure

take :: Signal a -> Time -> Signal a
take (Signal mEnd signal) length = Signal end' signal
  where
    end' = Finite $ case mEnd of
      Infinite -> length
      Finite end -> minTime end length

skip :: Time -> Signal a -> Signal a
skip length signal =
  Signal (mapLength (\ end -> maxTime 0 (end - length)) (end signal)) $ do
    runSignal <- initialize signal
    return $ \ time -> do
      runSignal (time + length)

withEnd :: Num a => Time -> Signal a -> Signal a
withEnd length signal = take (signal |> constant 0) length

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
zip a b = Signal (minLength (end a) (end b)) $ do
  runA <- initialize a
  runB <- initialize b
  return $ \ time -> do
    a <- runA time
    b <- runB time
    return (a, b)

zipWith :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipWith f a b = Signal (minLength (end a) (end b)) $ do
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

constSpeedup :: Double -> Signal a -> Signal a
constSpeedup (Time -> factor) signal =
  Signal (mapLength (/ factor) (end signal)) $ do
    runSignal <- initialize signal
    return $ \ time -> do
      runSignal (time * factor)

speedup :: Signal Double -> Signal a -> Signal a
speedup factorSignal inputSignal = case end inputSignal of
  Infinite -> Signal (end factorSignal) $ do
    runFactorSignal <- initialize $ integral factorSignal
    runInputSignal <- initialize inputSignal
    return $ \ time -> do
      integrated <- runFactorSignal time
      runInputSignal $ Time integrated
  Finite _ -> error "speedup not supported for finite input signals"

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

(|>) :: Signal a -> Signal a -> Signal a
a |> b = case end a of
  Infinite -> a
  Finite aEnd ->
    Signal (mapLength (+ aEnd) (end b)) $ do
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
  Infinite -> signal
  Finite end -> Signal Infinite $ do
    runSignal <- initialize signal
    return $ \ time ->
      runSignal $ Time (fromTime time `mod'` fromTime end)

infixr 6 +++
(+++) :: Num a => Signal a -> Signal a -> Signal a
a +++ b =
  let isValidTime :: Signal a -> Time -> Bool
      isValidTime signal time = case end signal of
        Infinite -> True
        Finite end -> time `lt` end
  in Signal (maxLength (end a) (end b)) $ do
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
