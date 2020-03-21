{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Signal (
  module Prelude,
  module Signal,
) where

import Data.Fixed
import Data.Vector (Vector, (!), (!?))
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
toByteString delta signal = Builder.toLazyByteString $ Vec.foldl' inner mempty $ toVector delta signal
  where
    inner :: Builder.Builder -> a -> Builder.Builder
    inner acc sample =
      acc <> Builder.lazyByteString (BS.toByteString sample) <> Builder.char7 '\n'

constant :: a -> Signal a
constant a = Signal Nothing $ \ times -> fmap (const a) times

take :: Double -> Signal a -> Signal a
take length (Signal mEnd signal) = Signal end' signal
  where
    end' = Just $ case mEnd of
      Nothing -> length
      Just end -> min end length

skip :: Double -> Signal a -> Signal a
skip length (Signal end signal) =
  Signal
    (fmap (\ signalEnd -> max 0 (signalEnd - length)) end)
    (\ times -> signal $ fmap (+ length) times)

fromList :: Double -> [a] -> Signal a
fromList delta (Vec.fromList -> vec) =
  Signal
    (Just $ fromIntegral (length vec) * delta)
    (fmap $ \ time -> vec ! floor (time / delta))

random :: forall a . Random a => (a, a) -> Signal a
random bounds =
  Signal Nothing $ \ times ->
    Vec.unfoldrN (Vec.length times) (\ gen -> Just (randomR bounds gen)) (mkStdGen 42)

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
speedup factorSignal inputSignal = case end inputSignal of
  Nothing -> Signal (end factorSignal) $ \ times ->
    runSignal inputSignal (integral times (runSignal factorSignal times))
  Just _ -> error "speedup not supported for finite input signals"

integral :: Vector Double -> Vector Double -> Vector Double
integral xs ys = Vec.constructN (Vec.length xs) $ \ acc ->
  case Vec.length acc of
    0 -> 0
    index -> lastArea + ((time - lastTime) * y)
      where
        lastArea = acc ! pred index
        time = xs ! index
        lastTime = xs ! pred index
        y = ys ! index

(|>) :: Signal a -> Signal a -> Signal a
a |> b = case end a of
  Nothing -> a
  Just aEnd ->
    Signal (fmap (+ aEnd) (end b)) $ \ times ->
      let (aTimes, bTimes) = Vec.span (`lt` aEnd) times
      in
        runSignal a aTimes <>
        runSignal b (fmap (subtract aEnd) bTimes)

repeat :: Integer -> Signal a -> Signal a
repeat n signal =
  if n <= 0
    then empty
    else signal |> repeat (n - 1) signal

(+++) :: Num a => Signal a -> Signal a -> Signal a
a +++ b =
  Signal (maxEnd a b) $ \ times ->
    zipWithOverlapping (+)
      (runSignal a $ takeValidTimes (end a) times)
      (runSignal b $ takeValidTimes (end b) times)
  where
    takeValidTimes :: Maybe Double -> Vector Double -> Vector Double
    takeValidTimes end times = maybe times (\ end -> Vec.takeWhile (`lt` end) times) end

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
