{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}

module Vommke where

import Control.Monad
import Control.Monad.Trans.State
import Data.List (foldl')
import Prelude ()
import Signal
import Signal.Core
import Signal.Notes
import Signal.Utils
import System.IO
import System.Random
import Utils

l = 0.3

vommke :: IO (Signal Double)
vommke = do
  gen <- newStdGen
  let motif = evalState (choose motifs) gen
  forM_ (parts motif) $ \ signal ->
    hPutStrLn stderr $ show $ end signal
  return $
    -- focus 55 500 $
    -- flip take 20 $
    song motif

song motif =
  mix (parts motif) |>
  silence 5 |>
  fart |>
  silence 5 |>
  empty

parts motif =
  crizzle motif :
  melody motif :
  bass motif :
  deep motif :
  []

crizzle motif =
  double $
  double $
  double $
  motif $
  motif $
  motif $
  note (l / 2) (base * 8)

melody motif =
  motif $
  motif $
  double $
  motif $
  double $
  note l (base * 4)

bass motif =
  motif $
  motif $
  motif $
  double $
  note (l * 2) (base * 2)

deep motif =
  motif $
  motif $
  motif $
  note (l * 4) base

base = fsharp

note len frequency =
  adsr len (Adsr 0.01 0.01 0.5 0.01) /\
  constSpeedup frequency saw

motifs :: [Signal Double -> Signal Double]
motifs = map (\ factors a -> foldl' (|>) empty $ map (\ factor -> constSpeedup factor a) factors) $
  [1, 1, 3 / 2, 1] :
  [1, 3 / 2, 1, 3 / 2] :
  [1, 4 / 3, 1, 3 / 2] :
  [1, 7 / 8, 1, 7 / 8] :
  [1, 3 / 2] :
  [1, 4 / 3, 4 / 3, 1] :
  []

double a =
  a |> a

fart =
  fmap (* 0.4) $
  adsr 4 (Adsr 0.2 3 0 0) /\
  speedup frequency (harmonics [1, 3, 0.4, 0.5, 0.3])
  where
    frequency =
      ramp 300 250 0.6 |>
      take (constant 250) 0.2 |>
      ramp 250 400 2.4
