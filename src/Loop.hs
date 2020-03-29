{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Loop where

import Bars
import Data.Function
import Data.List (foldl')
import Prelude ()
import Signal
import Transformations

main :: IO ()
main = do
  printSamples loop

bar = 3.2
beat = bar / 4
pointed = beat * 3 / 4

loop :: Signal Double
loop =
  fmap (* 0.2) $
  -- focus (bar * 10) (bar * 3.5) $
    ((ramp (bar * 4) 0 1 |> take (bar * 5.5) (constant 1)) /\ skip (bar * 4) arps) |>
    take (bar * 13.5) song |>
    (ramp (bar * 4) 1 0 /\ song) |>
    silence 5 |>
    empty

song =
  mix $
    fmap (* 0.06) melody :
    arps :
    snares :
    bass :
    chords :
    []


melody =
  inBars (bar * 4) $
    n 410 480 |> n 420 400 :
    n 410 480 |> n 420 400 :
    []
  where
    n start frequency = adsr 6.0 (Adsr 0.1 1 0.7 2.3) $
      speedup (take 4 (constant 1) |>
        fmap (project (-1, 1) (0.995, 1.005)) (speedup (constant (2 / fromTime beat)) sine)) $
      speedup (ramp 0.7 start frequency |> constant frequency) $
      rect

    inBars length signals = foldl' (|>) empty $ map (fill length) signals

data Part
  = A GC
  | B BB
  deriving (Eq, Generic)

instance AllValues Part

instance PartLength Part where
  partLength = \case
    A _ -> bar
    B sub -> case sub of
      BI -> bar * 2
      BII -> bar * 2.5
      BIII -> bar

data GC = G Fours | C Fours
  deriving (Eq, Generic)

instance AllValues GC

data Fours = One | Two | Three | Four
  deriving (Eq, Generic)

instance AllValues Fours

data BB = BI | BII | BIII
  deriving (Eq, Generic)

instance AllValues BB

arps =
  echo 0.31 0.5 $
    orchestrate arp
  where
    arp part =
      foldl' (\ acc frequency -> acc |> note frequency) empty $ fmap (* 200) $
        case part of
          A (G One) -> concat $ replicate 4 [1, 2, 1.5, 1.25]
          A (G Two) -> concat $ replicate 4 [1, 2, 1.5, 1.2]
          A (G Three) -> concat $ replicate 4 [1, 2, 1.5, 1.25]
          A (G Four) -> concat $ replicate 4 [1, 2, 1.5, 1.2]
          A (C One) -> fmap (* (4 / 3)) $
            [1, 2, 1.5, 1.25] ++
            [1, 2, 1.5, 1.25] ++
            [1, 2, 1.5, 1.25] ++
            [1, 2, 1.5, 1.25]
          A (C Two) -> fmap (* (4 / 3)) $
            [1, 2, 1.5, 1.2] ++
            [1, 2, 1.5, 1.2] ++
            [1, 1.2, 1.5, 1.5 * (9 / 8)] ++
            [1.5 * (5 / 4), 1.5 * (9 / 8), 1.5, 1.2]
          A (C Three) -> fmap (* (4 / 3)) $
            [1, 2, 1.5, 1.25] ++
            [1, 2, 1.5, 1.25] ++
            [1, 2, 1.5, 1.25] ++
            [1, 2, 1.5, 1.25]
          A (C Four) -> fmap (* (4 / 3)) $
            [1, 2, 1.5, 1.2] ++
            [1, 2, 1.5, 1.2] ++
            [1, 1.2, 1.5, 1.5 * (9 / 8)] ++
            [1.5 * (5 / 4), 2 * (9 / 8), 1.5 * (5 / 4), 1.5 * (9 / 8)]
          B BI ->
            [c', c'', g'] ++
            [f', d', bflat] ++
            [eflat', c'] ++
            concat (replicate 6 [aflat, aflat', eflat', c'])
          B BII ->
            [aflat, aflat', eflat'] ++
            [d', bflat, g] ++
            [c', aflat] ++
            [f, f', c', aflat] ++
            [f, f', c', aflat] ++
            [f, f', c', aflat] ++
            [f, f', c', aflat] ++
            [f, f', c', a] ++
            [f, f', c', a] ++
            [f, eflat', c', a] ++
            [f, eflat', c', a] ++
            []
          B BIII ->
            concat $ replicate 4 [fsharp, fsharp', d', a]

    d = d' / 2
    f = f' / 2
    fsharp = d * 5 / 4
    g = 1
    aflat = bflat * 7 / 8
    a = f * 5 / 4
    bflat = g * 1.2
    c' = 4 / 3
    d' = g * 3 / 2
    eflat' = 1.2 * 4 / 3
    f' = c' * 4 / 3
    fsharp' = d' * 5 / 4
    g' = 2
    aflat' = aflat * 2
    c'' = c' * 2

    note :: Double -> Signal Double
    note frequency =
      adsr (beat / 4) (Adsr 0.001 0.1 0.3 0.05) $
        speedup (constant frequency) $ fmap sin phase

snares =
  echo 0.105 0.05 $ orchestrate $ \case
    A (G One) -> silence 0.8 |> snare # silence 0.8 |> snare
    A (G Two) -> (silence 0.8 |> snare |> silence 0.5 |> snare) # (silence 0.8 |> snare)
    A (G Three) -> silence 0.8 |> snare # silence 0.8 |> fill 0.4 snare |> snare
    A (G Four) -> silence 0.8 |> fill 0.6 snare |> snare # silence 0.6 |> fill 0.6 snare |> snare
    A (C One) -> silence 0.8 |> snare # silence 0.8 |> snare
    A (C Two) -> silence 0.8 |> snare |> silence 0.5 |> snare # silence 0.8 |> snare
    A (C Three) -> silence 0.8 |> snare # silence 0.8 |> fill 0.4 snare |> snare
    A (C Four) -> silence 0.8 |> fill 0.6 snare |> snare # silence 0.6 |> fill 0.6 snare |> snare
    B BI ->
      silence (beat / 2) |> fill pointed snare |> fill pointed snare |>
      (silence 0.8 |> snare # (silence 0.8 |> snare # silence 0.8 |> snare))
    B BII ->
      silence (beat / 2) |> fill pointed snare |> fill pointed snare |>
      (silence 0.8 |> snare # fill (2 * beat) (silence 0.8 |> snare)) |>
      (silence 0.8 |> snare # silence 0.8 |> snare)
    B BIII -> silence 0.8 |> fill 0.6 snare |> snare # silence 0.6 |> fill 0.6 snare |> snare
  where
    a # b = fill (2 * beat) a |> b
    snare =
      random (-1, 1)
        & adsr 0.06 (Adsr 0.01 0.05 0.2 0.01)
        & fmap (* 0.3)

bass =
  skip 0.02 $ orchestrate part
  where
    part :: Part -> Signal Double
    part part = case part of
      A (G One) -> fill 3 (n g) |> n f
      A (G Two) -> n g
      A (G Three) -> fill 3 (n g) |> n f
      A (G Four) -> n g |> n f |> n g
      A (C One) -> fill 3 (n c) |> n bflat
      A (C Two) -> n c
      A (C Three) -> fill 3 (n c) |> n bflat
      A (C Four) -> n c |> n bflat |> n c
      B BI -> fill (bar / 2) $
        fill pointed (n c) |> fill pointed (n bflat) |> n aflat
      B BII ->
        fill pointed (n aflat) |> fill pointed (n g) |>
        fill (beat * 0.5) (n f) |> silence (beat * 3.75) |> fill (beat * 0.25) (n eflat) |>
        n f
      B BIII -> fill bar $
        n fsharp

    eflat = f * 7 / 8
    f = g * 7 / 8
    fsharp = ((50 * 3 / 2) * 5 / 4) / 2
    g = 50
    aflat = bflat * 7 / 8
    bflat = g * 1.2
    c = g * 4 / 3

    n frequency =
      adsr 0.2 (Adsr 0.01 0.06 0.5 0.01) $
      speedup (constant frequency +++ (take (beat / 8) (constant 0) |> ramp (beat / 8) 0 (-7))) $
      fmap (clip (-1, 1) . (* 7)) $
      saw

chords = fmap (* 0.13) $ orchestrate $ \case
  B BI ->
    fill pointed (n eflat' +++ n c') |>
    fill pointed (n d' +++ n bflat) |>
    adsr (bar * 7 / 4) (Adsr 0 0 1 (bar / 2)) (ln True c' +++ ln False aflat')
  B BII ->
    fill pointed (n aflat' +++ n eflat') |>
    fill pointed (n g' +++ n d') |>
    adsr (bar * 3.125) (Adsr 0 0 1 (bar / 2)) (
      lns True (
        take (beat * 0.5 + beat * 4 + beat * 3.5) (constant f') |>
        ramp (beat * 1) f' fsharp' |>
        constant fsharp' |>
        constant 0
      ) +++
      lns False (
        take (beat * 4) (constant aflat') |>
        ramp (beat * 1) aflat' a' |> take (beat * 3) (constant a') |>
        ramp (beat * 1) a' c'' |>
        constant c'' |>
        constant 0
      )
    ) |>
    empty
  _ -> empty
  where
    g = 200
    bflat = g * 1.2
    c' :: Double
    c' = g * 4 / 3
    d' = bflat * 5 / 4
    eflat' = c' * 1.2
    f' = g' * 7 / 8
    fsharp' = d' * 5 / 4
    g' = g * 2
    aflat' = 2 * bflat * 7 / 8
    a' = f' * 5 / 4
    c'' = c' * 2

    n frequency =
      adsr 0.4 (Adsr 0.05 0.0 1 0.05) $
      wave (constant frequency)

    ln :: Bool -> Double -> Signal Double
    ln cos frequency = lns cos (constant frequency)

    lns :: Bool -> Signal Double -> Signal Double
    lns cos frequency =
      (tremolo cos /\) $
      speedup (ramp (bar / 4) 0.9 1 |> constant 1) $
      wave frequency

    tremolo cos =
      zipWith
        (\ lowLevel wave -> project (-1, 1) (lowLevel, 1) wave)
        tremoloLowLevel
        (constSpeedup (fromTime (16 / bar)) (if cos then skip 0.5 rect else rect))

    tremoloLowLevel =
      take (bar / 4) (constant 1) |>
      ramp (bar / 2) 1 tremoloMinLevel |>
      constant tremoloMinLevel
    tremoloMinLevel = 0.5

    wave frequency =
      speedup frequency rect
