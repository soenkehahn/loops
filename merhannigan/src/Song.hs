{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Song where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans.State
import Data.List (sort, zip)
import Data.Ratio
import Prelude ()
import Signal
import Signal.Core
import Signal.Memoize
import Signal.Snippet
import Signal.Utils
import System.Random
import Utils

song :: IO (Signal Double)
song = do
  gen <- getStdGen
  let tracks = evalState (randomSong 110) gen
  return $
    -- focus (l * 48) 1000 $
    -- flip take (l * 16) $
    tracks

randomSong :: Double -> State StdGen (Signal Double)
randomSong base = do
  a <- randomPart (base * 2) base False
  b <- randomPart (base * 2) (base * 4 / 3) False
  c <- randomPart (base * 2) (base * 3 / 2) False
  d <- randomPart (base * 2) base True
  return $ raster (l * 16) (map (1 .>) [a, b, c, d])

randomPart :: Double -> Double -> Bool -> State StdGen (Signal Double)
randomPart octave base end = do
  ticks <- mkTicks
  gongs <- mkGongs base end
  snares <- mkSnares
  melody <- mkMelody octave (base * 2) (l * 16) end
  return $ mix $
    d ticks :
    d gongs :
    d snares :
    melody :
    []

  where
    d :: Signal Double -> Signal Double
    d signal = silence (l * 0.01) |> signal

l = 1

mkGongs base end = do
  gong <- mkGong
  return $ raster (l * 4) (replicate (if end then 5 else 4) (1 .> gong))
  where
    mkGong = do
      numberOfHarmonics <- choose [1 .. 20]
      harmonicWeights <- forM [1 .. numberOfHarmonics] $ \ n ->
        range (0, 2 ** (- n))
      tremolo <- mkTremolo
      frequency <- mkFrequency
      return $
        fmap (* 0.1) $
        (env /\ tremolo) /\
        speedup frequency (harmonics harmonicWeights)
    env = adsr (l * 5) (Adsr 0.02 ((l * 5) - 0.02) 0 0)
    mkTremolo = do
      speed <- choose [1 .. 6]
      tremDeviation <- range (0, 0.1)
      return $
        constSpeedup speed $
        fmap (project (-1, 1) (1 - tremDeviation, 1 + tremDeviation)) sine
    mkFrequency = do
      speed <- choose [1 .. 6]
      deviation <- range (1, 1.01)
      return $
        fmap (project (-1, 1) (base, base * deviation)) $
        constSpeedup speed $
        sine

mkTicks :: State StdGen (Signal Double)
mkTicks = do
  withRandomPatterns l 16 mkTick $
    (1, [1, 0.2, 0.5, 0.9, 0.2, 0.5]) :
    (1, [1, 0.2, 0.8, 0.2, 0.9, 0.2]) :
    []

mkTick = do
  factor <- randomInState $ randomR (0.6, 1.3)
  return $
    fmap (* 0.04) $
    fmap (* factor) $
    adsr 1 (Adsr ramp ramp 0 0) /\
    noise (-1, 1)
  where
    ramp = 0.005


mkSnares :: State StdGen (Signal Double)
mkSnares = do
  snare <- mkSnare
  withRandomPatterns (l * 2) 8 snare $
    (1, [0, 1]) :
    (1, [0, 0, 0, 0.5, 0, 0, 1, 0, 0, 0, 0, 1]) :
    (1, [0, 0, 0.5, 0, 0, 0.5, 0, 0, 0.5, 0, 0, 0.5]) :
    []

mkSnare = do
  highSines <- replicateM 8 $ do
    frequency <- range (10000, 18000)
    volume <- range (0, 1)
    return $
      fmap (* volume) $
      constSpeedup frequency rect
  lowSines <- do
    frequency <- range (30, 100)
    frequencies <- forM [1, 0.75 .. 0] $ \ n ->
      range (0, n)
    return $
      constSpeedup frequency $
      harmonics frequencies
  return $ do
    volume <- range (0.3, 1.3)
    return $
      fmap (* 0.1) $
      adsr 1 (Adsr 0.01 0.08 0 0) /\
      memoizeWave (
        fmap (* volume) $
        mixWithVolumes $
          (0.05, mix highSines) :
          (0.5, lowSines) :
          (0.2, noise (-1, 1)) :
          [])

withRandomPatterns :: Time -> Int -> State StdGen (Signal Double) -> [(Int, [Rational])]
  -> State StdGen (Signal Double)
withRandomPatterns length replication mkSignal patterns = do
  parts <- forM [1 .. replication] $ const once
  return $ raster length (fmap (1 .>) parts)
  where
    once = do
      pattern :: [Bool] <- mkPattern =<< weighted patterns
      beats <- forM pattern $ \ inPattern ->
        if inPattern
          then mkSignal
          else return empty
      return $ flip evenly length $ map const beats

    mkPattern :: [Rational] -> State StdGen [Bool]
    mkPattern = mapM inner
      where
        inner :: Rational -> State StdGen Bool
        inner weight = weighted $ fmap (first fromIntegral) $
          (numerator weight, True) :
          (denominator weight - numerator weight, False) :
          []

pickFromScale scale = weighted $ Data.List.zip weights scale
  where
    weights =
      10 :
      3 :
      10 :
      3 :
      10 :
      6 :
      3 :
      10 :
      []

mkScale :: Double -> Int -> [Double]
mkScale base offset =
  sort $
  map (limitPitch base) $
  (base * 2 :) $
  map (\ position -> base * (3 ** fromIntegral position)) $
  [-offset .. -offset + 6]

limitPitch octave frequency =
  if frequency > octave * 2
  then limitPitch octave (frequency / 2)
  else if frequency < octave
  then limitPitch octave (frequency * 2)
  else frequency

mkMelody :: Double -> Double -> Time -> Bool -> State StdGen (Signal Double)
mkMelody octave base time end = do
  notes <- runGrammar $
    rule 1 (\ [S] -> [NT 1]) :
    guarded 4 (\ [NT duration] ->
      (duration > 0.025, [NT (duration / 2), NT (duration / 2)])) :
    guarded 4 (\ [NT duration] ->
      (duration < 0.125 && duration > 0.025, [NT (duration / 3), NT (duration / 3), NT (duration / 3)])) :
    guarded 1 (\ [NT duration] -> (duration < 0.125, [T duration])) :
    []
  let scale = mkScale base 0
  parts <- forM notes $ \ duration -> do
    frequency <- pickFromScale scale
    return $ duration ~> n (limitPitch octave frequency)
  return $
    fmap (* 0.04) $
    divide parts time |>
    if end then n base (l * 2) else empty

  where
    n frequency duration =
      adsr (duration * 0.75) (Adsr 0.01 0 1 0.1) /\
      constSpeedup frequency (harmonics [1, 0.05])
