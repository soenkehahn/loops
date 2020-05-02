{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}

module Vommke where

import Signal
import Signal.Core
import Signal.Notes
import Signal.Utils
import Prelude ()
import Control.Monad
import System.IO

l = 0.3

vommke :: IO (Signal Double)
vommke = do
  forM_ parts $ \ signal ->
    hPutStrLn stderr $ show $ end signal
  return $
    -- focus 55 500 $
    song

song =
  mix parts |>
  silence 5 |>
  fart |>
  silence 5

parts =
  crizzle :
  melody :
  bass :
  deep :
  []

crizzle =
  double $
  double $
  double $
  motif $
  motif $
  motif $
  note (l / 2) (base * 8)

melody =
  motif $
  motif $
  double $
  motif $
  double $
  note l (base * 4)

bass =
  motif $
  motif $
  motif $
  double $
  note (l * 2) (base * 2)

deep =
  motif $
  motif $
  motif $
  note (l * 4) base

base = fsharp

note len frequency =
  adsr len (Adsr 0.01 0.01 0.5 0.01) /\
  constSpeedup frequency saw

motif a =
  a |> a |> constSpeedup (3 / 2) a |> a

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
