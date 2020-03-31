{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Loop where

import Data.Function
import Prelude ()
import Signal
import Snippet
import Loop.Song

main :: IO ()
main = do
  printSamples loop

loop :: Signal Double
loop =
  fmap (* 0.2) $
  -- constSpeedup 0.8 $
  -- focus (secondVerse + bar * 7) (bar * 6.5) $
  runSnippet $
    mempty
    <> 0 |-> intro
    <> songStart |-> song
    <> secondVerse |->
      song +++
      percussion
    <> outro |-> outroPart |> silence 5
  where
    intro =
      (ramp (bar * 4) 0 1 |> take (bar * 5.5) (constant 1)) /\ skip (bar * 4) arps

song =
  mix $
    fmap (* 0.06) melody :
    arps :
    snares :
    bass :
    chords :
    []

percussion =
  fmap (* 0.13) $
  runSnippet $
    mempty
    <> 0 |->
      repeat 3
        (rhythm tchak |> rhythm tchak)
    <> (bar * 2.5) |->
      (ramp (bar / 2) 0 1 /\ kicks) |>
      take (bar * (5 + 3.5)) (cycle kicksHalfBeat) |>
      (ramp (bar * 1.5) 1 0 /\ cycle kicksHalfBeat)

  where
    rhythm signal =
      fill (bar / 2) $
      runSnippet $
        (beat / 2) |-> signal <>
        (beat * (5 / 6)) |-> fmap (* 0.8) signal <>
        (beat * (7 / 6)) |-> fmap (* 0.4) signal <>
        (beat * (9 / 6)) |-> signal <>
        mempty

    tchak =
      adsr 0.4 (Adsr 0.0001 0.1 0 0) $
        random (-1, 1) +++
        s 12000 +++
        s 8000 +++
        s 5000 +++
        s 500 +++
        empty

    s frequency =
      speedup (ramp 0.1001 frequency (frequency * 0.93)) sine

kicksHalfBeat =
  kick |>
  fmap (* 0.1) kick |>
  fmap (* 0.5) kick |>
  fmap (* 0.6) kick
kicks = cycle kick
kick =
  fmap (* 0.6) $
  fill (beat / 8) $
  adsr 0.3 (Adsr 0.0001 0.05 0 0) $
    random (-1, 1) +++
    constSpeedup 17000 rect +++
    constSpeedup 16000 rect +++
    constSpeedup 15000 rect

outroPart =
  env /\ signal
  where
    env =
      ramp (bar * 2) 1 0.3 |>
      take (bar * 4) (constant 0.3) |>
      ramp (bar * 6) 0.3 0

    signal = runSnippet $
      mempty
      <> 0 |->
        arpsEcho (cycle (take (bar * 4) arpsWithoutEcho))
      <> (bar * 3.5) |->
        fmap (* 0.05)
        (ramp (bar / 2) 0 1 /\ kicks |>
         cycle kicksHalfBeat)
