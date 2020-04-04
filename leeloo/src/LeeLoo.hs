{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}

module LeeLoo where

import Signal
import Signal.Snippet
import Signal.Utils
import Signal.Notes
import Signal.Transformations
import Prelude ()

l :: Time -> Time
l n = n * 3

leeloo :: Signal Double
leeloo =
  fmap (* 0.2) $
  focus 0 (l 4) $
  silence 0.05 |> chords +++
  melody +++
  drums +++
  -- tiktok +++
  empty

melody =
  fmap (* 0.1) $
  raster (l 1 / 12) [
    2 ~> sn a''',
    4 ~> n c'''',
    2 ~> sn d'''',
    2 ~> n c'''',
    11 ~> ns (divide [
      2 ~> ramp a''' aflat''',
      1 ~> ramp aflat''' a''',
      8 ~> \ _ -> constant a'''
     ]),
    3 ~> divide [
      2 ~> ns (\ t -> ramp (pitch (-0.5) aflat''') g''' t |> constant g'''),
      1 ~> n f'''
    ],
    11 ~> n d''',
    1 ~> n c''',
    1 ~> n d''',
    1 ~> n c''',
    10 ~> n d'''
  ]

  where
    n :: Double -> Time -> Signal Double
    n frequency length =
      nadsr length $
      constSpeedup frequency wave

    sn frequency =
      ns $ \ len ->
        take (len * 1.5) $
        ramp (pitch (-1) frequency) frequency 0.3 |> constant frequency

    ns :: (Time -> Signal Double) -> Time -> Signal Double
    ns frequency length =
      nadsr length $
      speedup (frequency length) wave

    nadsr length =
      adsr (length + 0.3) (Adsr 0.02 0.2 0.5 0.3)

    wave = harmonics [1, 0.8, 1, 0.5, 0.9, 0.3, 0.2, 0.1]


chords =
  phaser $
  fmap (* 0.05) $
    empty
    +++ l 0 |-> partA
    +++ l 8 |-> partA
    +++ l 16 |-> partB
    +++ l 30 |-> partA

partA =
  empty
  +++ l 0 |->
    chord [f'', c''', e''', a''']
  +++ l 1 |->
    chord [f'', c''', e''', a''']
  +++ l 2 |->
    chord [bflat', d''', f''', a''']
  +++ l 3 |->
    chord [bflat', d''', f''', a''']
  +++ l 4 |->
    chord [f'', c''', e''', a''']
  +++ l 5 |->
    chord [c'', bflat'', e''', g''']
  +++ l 6 |->
    chord [f'', c''', e''', a''']
  +++ l 7 |->
    chord [c'', bflat'', e''', g''']

partB =
  empty
  +++ l 0 |-> chord [bflat', a'', d''', f''']
  +++ l 1 |-> chord [b', aflat'', d''', f''']
  +++ l 2 |-> chord [c'', a'', e''', f''']
  +++ l 3 |-> chord [c'', a'', eflat''', f''']
  +++ l 4 |-> chord [bflat', a'', d''', f''']
  +++ l 5 |-> chord [bflat', aflat'', dflat''', f''']
  +++ l 6 |-> chord [a', g'', c''', e''']
  +++ l 7 |-> chord [d'', fsharp'', c''', e''']
  +++ l 8 |-> chord [bflat', a'', d''', f''']
  +++ l 9 |-> chord [bflat', aflat'', dflat''', f''']
  +++ l 10 |-> chord [a', g'', c''', e''']
  +++ l 11 |-> chord [d'', fsharp'', c''', e''']
  +++ l 12 |-> chord [g', f'', bflat'', d''']
  +++ l 13 |-> chord [c'', e'', bflat'', d''']

chord frequencies =
  fanOut (adsr (l 1 * 1.1) (Adsr 0.01 0.2 0.7 1) . note) frequencies

note frequency = constSpeedup frequency $ harmonics [1, 0.5, 0.9, 0.3, 0.6]

tiktok :: Signal Double
tiktok =
  fmap (* 0.2) $
  cycle $
  fill (l (1 / 4)) $ take 0.001 (constant 1)

phaser :: Signal Double -> Signal Double
phaser = onFinite inner
  where
    onFinite :: (Signal Double -> Signal Double) -> (Signal Double -> Signal Double)
    onFinite onInfinite signal = case end signal of
      Nothing -> onInfinite signal
      Just end ->
        take end (onInfinite (signal |> constant 0))

    phaseSignal =
      skip 0.5 $
      fmap (project (-1, 1) (1 - deviation, 1 + deviation)) $
      constSpeedup (fromTime (1 / frequency)) $
      sine

    inner signal =
      fmap (/ (1 + wetness)) $
        signal +++
        fmap (* wetness) (speedup phaseSignal signal)

    wetness = 1

    deviation = 0.003

    frequency = l 1 / 4

drums :: Signal Double
drums =
  evenly (
    kick :
    kick :
    snare :
    snare :
    kick :
    kick :
    snare :
    evenly [snare, kick] :
    kick :
    kick :
    snare :
    snare :
    kick :
    kick :
    snare :
    evenly [snare, kick] :
    []) (l 4)

  where
    kick _len =
      fmap (compress 0.999) $
      env len /\
      speedup (ramp 45 20 len) sine

    len = 0.08

    env :: Time -> Signal Double
    env len = Signal (Just len) $ return $ \ t ->
      return $ 1 - (((fromTime t - tweak) / tweak) ** 2)
      where
        tweak = fromTime len / 2

    snare _len =
      fmap (* 0.35) $
      echo 0.1 0.1 $
      adsr 0.15 (Adsr 0.01 0.1 0.1 0.01)
      (random (-1, 1) +++
       fmap (* 0.25) (speedup (ramp 190 100 0.21) (harmonics [1, 0.2])))

compress :: Double -> Double -> Double
compress 0 x = x
compress _ 0 = 0
compress tweak x | x > 0 = - (((1 - tweak) ** x) - 1) / tweak
compress tweak x = - compress tweak (- x)
