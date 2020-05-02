{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Song where

import Signal
import Signal.Core
import Utils
import Signal.Snippet
import Signal.Utils
import Prelude ()
import Control.Monad
import Control.Monad.Trans.State
import System.Random

song :: IO (Signal Double)
song = do
  gen <- getStdGen
  return $
    -- focus 0 4 $
    evalState beats gen +++
    gongs

l = 1

gongs = raster l (replicate 8 (1 .> gong))
  where
    gong =
      fmap (* 0.02) $
      adsr 3 (Adsr 0.02 2.9 0 0) /\
      constSpeedup 110 (harmonics ([1, 1, 0.8, 1, 1, 0.3, 0.6, 0.3, 0.2, 0, 0.2]))

beats :: State StdGen (Signal Double)
beats = do
  bars <- forM [1 .. 8 :: Int] $ const bar
  return $ raster l (map (1 .>) bars)

bar = do
  numberOfBeats :: Int <- weighted $
    (1, 3) :
    (4, 4) :
    (1, 6) :
    []
  pattern :: [Bool] <- forM [1 .. numberOfBeats] $ const $ randomInState $ random
  beats <- forM pattern $ \ inPattern ->
    if inPattern
      then beat
      else return empty
  return $ flip evenly l $ map const beats


beat = do
  factor <- randomInState $ randomR (0.7, 1.3)
  return $
    fmap (* 0.02) $
    fmap (* factor) $
    adsr 1 (Adsr 0.01 0.4 0 0) /\
    noise (-1, 1)
