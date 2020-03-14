{-# LANGUAGE DeriveFunctor #-}

module Signal where

import Control.Arrow
import Prelude hiding (take)
import System.Random
import Data.Fixed

-- signal basics

data Signal a = Signal {
  runSignal :: Double -> Maybe (a, Signal a)
} deriving (Functor)

stateful :: state -> (Double -> state -> Maybe (a, state)) -> Signal a
stateful current f = Signal $ \ delta ->
  case f delta current of
    Just (a, nextState) -> Just (a, (stateful nextState f))
    Nothing -> Nothing

instance Applicative Signal where
  pure = constant
  fSignal <*> xSignal = Signal $ \ delta -> do
    (f, nextFSignal) <- runSignal fSignal delta
    (x, nextXSignal) <- runSignal xSignal delta
    return (f x, nextFSignal <*> nextXSignal)

toList :: Double -> Signal a -> [a]
toList delta signal = case runSignal signal delta of
  Just (sample, next) -> sample : toList delta next
  Nothing -> []

constant :: a -> Signal a
constant a = Signal $ \ delta -> Just (a, (constant a))

take :: Double -> Signal a -> Signal a
take length (Signal signal) =
  if length < 0
    then Signal $ \ delta -> Nothing
    else Signal $ \ delta -> case signal delta of
      Just (sample, next) -> Just (sample, (take (length - delta) next))

fromList :: [a] -> Signal a
fromList list = stateful list $ \ delta list -> case list of
  a : r -> Just (a, r)
  [] -> Nothing

random :: Random a => (a, a) -> Signal a
random bounds = fromList $ randomRs bounds (mkStdGen 42)

-- audio signals

tau :: Double
tau = pi * 2

phase :: Signal Double
phase = stateful 0 $ \ delta phase -> Just (phase, (phase + delta * tau) `mod'` tau)

project :: (Double, Double) -> (Double, Double) -> Double -> Double
project (fromLow, fromHigh) (toLow, toHigh) x =
  (((x - fromLow) / (fromHigh - fromLow)) * (toHigh - toLow)) + toLow

speedup :: Signal Double -> Signal a -> Signal a
speedup factorSignal inputSignal = Signal $ \ delta -> do
  (factor, nextFactorSignal) <- runSignal factorSignal delta
  (sample, nextInputSignal) <- runSignal inputSignal (factor * delta)
  return (sample, speedup nextFactorSignal nextInputSignal)
