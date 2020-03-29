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
  -- constSpeedup 0.5 $
  -- focus secondVerse (bar * 2) $
  runSnippet $
    mempty
    <> 0 |-> intro
    <> songStart |-> song
    <> secondVerse |->
      song +++
      percussion
    <> outro |-> (ramp (bar * 4) 1 0 /\ song) |> silence 5
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
    <> 0
      |-> rhythm tchak |> rhythm tchak
    <> bar
      |-> rhythm tchak |> rhythm tchak
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
