{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal (
  module Prelude,
  module Signal,
) where

import Control.Monad.Trans.State.Strict
import Data.Fixed
import Data.Vector (Vector, (!?))
import Epsilon
import Prelude hiding (take, repeat)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Conversion as BS
import qualified Data.ByteString.Lazy as BS hiding (snoc)
import qualified Data.Vector as Vec
import System.Random

-- signal basics

data Signal a = Signal {
  end :: Maybe Double,
  runSignal :: Vector Double -> Vector a
} deriving (Functor)

minEnd :: Signal a -> Signal b -> Maybe Double
minEnd a b = case (end a, end b) of
  (Just a, Just b) -> Just $ min a b
  (Just a, Nothing) -> Just a
  (Nothing, Just b) -> Just b
  (Nothing, Nothing) -> Nothing

maxEnd :: Signal a -> Signal b -> Maybe Double
maxEnd a b = case (end a, end b) of
  (Just a, Just b) -> Just $ max a b
  (_, Nothing) -> Nothing
  (Nothing, _) -> Nothing

instance Applicative Signal where
  pure = constant
  fSignal <*> xSignal =
    Signal (minEnd fSignal xSignal) $ \ times ->
      Vec.zipWith ($)
        (runSignal fSignal times)
        (runSignal xSignal times)

toVector :: Double -> Signal a -> Vector a
toVector delta signal =
  runSignal signal deltas
  where
    deltas :: Vector Double
    deltas = case end signal of
      Just end ->
        let numberOfSamples = ceiling (end / delta)
        in Vec.enumFromStepN 0 delta numberOfSamples
      Nothing -> error "toVector: infinite signals are not supported"

toByteString :: forall a . BS.ToByteString a => Double -> Signal a -> BS.ByteString
toByteString delta signal = Builder.toLazyByteString $ inner $ Vec.toList $ toVector delta signal
  where
    inner :: [a] -> Builder.Builder
    inner samples = case samples of
      sample : r ->
        Builder.lazyByteString (BS.toByteString sample) <> Builder.char7 '\n' <>
        inner r
      [] -> mempty

constant :: a -> Signal a
constant a = Signal Nothing $ fmap $ const a

take :: Double -> Signal a -> Signal a
take length signal = Signal end' (runSignal signal)
  where
    end' = Just $ case end signal of
      Nothing -> length
      Just e -> min e length

skip :: Double -> Signal a -> Signal a
skip length signal =
  let e = case end signal of
        Just signalEnd -> Just $ max 0 (signalEnd - length)
        Nothing -> Nothing
  in Signal e $ \ times -> runSignal signal $ fmap (+ length) times

fromList :: Double -> [a] -> Signal a
fromList delta list =
  Signal
    (Just $ fromIntegral (length list) * delta)
    (fmap $ \ time -> list !! floor (time / delta))

random :: forall a . Random a => (a, a) -> Signal a
random bounds = Signal Nothing $ \ times -> evalState (mapM inner times) (mkStdGen 42)
  where
    inner :: Double -> State StdGen a
    inner _ = do
      gen <- get
      let (a, newGen) = randomR bounds gen
      put newGen
      return a

empty :: Signal a
empty = Signal (Just 0) (fmap (\ _time -> error "empty: shouldn't be called"))

-- audio signals

tau :: Double
tau = pi * 2

phase :: Signal Double
phase = Signal Nothing $ fmap $ \ time -> (time `mod'` 1) * tau

project :: (Double, Double) -> (Double, Double) -> Double -> Double
project (fromLow, fromHigh) (toLow, toHigh) x =
  (((x - fromLow) / (fromHigh - fromLow)) * (toHigh - toLow)) + toLow

clip :: (Double, Double) -> Double -> Double
clip (lower, upper) x = max lower (min upper x)

speedup :: forall a . Signal Double -> Signal a -> Signal a
speedup factorSignal (Signal Nothing inputSignal) =
  Signal (end factorSignal) $ \ times ->
    inner (integral $ Vec.zip times (runSignal factorSignal times))
  where
    inner :: Vector Double -> Vector a
    inner = inputSignal
speedup _ (Signal (Just _) _) = error "speedup not supported for finite input signals"

integral :: Vector (Double, Double) -> Vector Double
integral foo = evalState (mapM inner foo) (Nothing, 0)
  where
    inner :: (Double, Double) -> State (Maybe Double, Double) Double
    inner (time, factor) = do
      (oldTime, current) <- get
      case oldTime of
        Nothing -> do
          put (Just time, current)
          return current
        Just oldTime -> do
          let new = current + ((time - oldTime) * factor)
          put (Just time, new)
          return new

(|>) :: Signal a -> Signal a -> Signal a
a |> b = case end a of
  Nothing -> a
  Just aEnd ->
    let e = case end b of
          Nothing -> Nothing
          Just e -> Just (aEnd + e)
    in Signal e $ \ times ->
      let (forA, forB) = Vec.span (< aEnd) times
      in runSignal a forA <> runSignal b (fmap (subtract aEnd) forB)

repeat :: Integer -> Signal a -> Signal a
repeat n signal =
  if n <= 0
    then empty
    else signal |> repeat (n - 1) signal

(+++) :: Num a => Signal a -> Signal a -> Signal a
a +++ b =
  Signal (maxEnd a b) $ \ times ->
    let as = runSignal a $ maybe times (\ end -> Vec.takeWhile (`lt` end) times) (end a)
        bs = runSignal b $ maybe times (\ end -> Vec.takeWhile (`lt` end) times) (end b)
    in zipWithOverlapping (+) as bs

zipWithOverlapping :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
zipWithOverlapping f as bs =
  Vec.generate (max (length as) (length bs)) $ \ i ->
    case (as !? i, bs !? i) of
      (Just a, Just b) -> f a b
      (Just a, Nothing) -> a
      (Nothing, Just b) -> b
      (Nothing, Nothing) -> error "zipWithOverlapping: shouldn't happen"

(/\) :: Num a => Signal a -> Signal a -> Signal a
a /\ b = (*) <$> a <*> b

silence :: Num a => Double -> Signal a
silence length = take length (constant 0)

fill :: Num a => Double -> Signal a -> Signal a
fill length signal = take length (signal |> constant 0)

saw :: Signal Double
saw = fmap (project (0, tau) (-1, 1)) phase

sine :: Signal Double
sine = fmap sin phase

rect :: Signal Double
rect = fmap (\ x -> if x < tau / 2 then -1 else 1) phase

ramp :: Double -> Double -> Double -> Signal Double
ramp 0 _ _ = empty
ramp length start end =
  Signal (Just length) $ fmap $ \ time ->
    (time / length) * (end - start) + start

data Adsr = Adsr {
  attack :: Double,
  decay :: Double,
  sustain :: Double,
  release :: Double
}

adsr :: Double -> Adsr -> Signal Double -> Signal Double
adsr length (Adsr attack decay sustain release) signal =
  envelope /\ signal
  where
    envelope =
      ramp attack 0 1 |>
      ramp decay 1 sustain |>
      take (length - attack - decay - release) (constant sustain) |>
      ramp release sustain 0
