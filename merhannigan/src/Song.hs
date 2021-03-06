{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Song where

import Signal
import Control.Arrow (first)
import Signal.Core
import Signal.Memoize
import Utils
import Data.Ratio
import Signal.Snippet
import Signal.Utils
import Prelude ()
import Control.Monad
import Control.Monad.Trans.State
import System.Random
import System.IO

song :: IO (Signal Double)
song = do
  gen <- getStdGen
  let tracks = evalState randomSong gen
  forM_ tracks $ \ track ->
    hPutStrLn stderr $ show $ end track
  return $
    mix tracks

randomSong = do
  beats <- mkBeats
  gongs <- mkGongs
  snares <- mkSnares
  return $
    beats :
    gongs :
    snares :
    []

l = 1

mkGongs = do
  gong <- mkGong
  return $ raster (l * 4) (replicate 4 (1 .> gong))
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
    base = 110

mkBeats :: State StdGen (Signal Double)
mkBeats = do
  withRandomPatterns l 16 mkBeat $
    (1, [1, 0.5, 0.5, 0.9, 0.5, 0.5]) :
    (1, [1, 0.5, 0.9, 0.5, 0.9, 0.5]) :
    []

mkBeat = do
  factor <- randomInState $ randomR (0.6, 1.3)
  return $
    fmap (* 0.02) $
    fmap (* factor) $
    adsr 1 (Adsr 0.01 0.02 0 0) /\
    noise (-1, 1)


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
