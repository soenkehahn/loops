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
  fmap (* 0.6) $
  focus (l 14) (l 6) $
  silence 0.03 |> chords +++
  melody +++
  drums +++
  empty

melody =
  fmap (* 0.1) $
  raster (l 1) [
    8 .> a1,
    8 .> a2
  ]
  where
    a1 = raster (l 1 / 12) [
        2 ~> sn a''',
        4 ~> n c'''',
        2 ~> sn d'''',
        2 ~> n c'''',
        11 ~> ns (divide [
          2 ~> ramp a''' aflat''',
          1 ~> ramp aflat''' a''',
          8 .> constant a'''
        ]),
        3 ~> divide [
          2 ~> ns (\ t -> ramp (pitch (-0.5) aflat''') g''' t |> constant g'''),
          1 ~> n f'''
        ],
        11 ~> n d''',
        1 ~> n c''',
        0.75 ~> n d''',
        0.75 ~> n c''',
        10.5 ~> n d''',
        10 .> empty,
        1 ~> n c''',
        1 ~> n d''',

        2 ~> sn e''',
        9 ~> sn e''',
        1 ~> n d''',

        2 ~> evenly (map n [e''', g''', e''', d''']),
        6 ~> n e''',
        1 ~> n c''',
        2.4 ~> divide [
          1 ~> n eflat''',
          1 ~> n dflat''',
          1 ~> n c''',
          1 ~> n bflat''],

        12.6 ~> n c'''
      ]

    a2 = raster (l 1 / 12) [
        2 ~> sn a''',
        4 ~> n c'''',
        2 ~> n d'''',
        2 ~> n c'''',
        1 .> empty,
        10 ~> n a''',
        2 .> empty,
        1 ~> n f''',
        11 ~> n d''',
        1 ~> n c''',
        12 ~> n d''',

        11 .> empty,
        1 ~> n d''',
        11 ~> sn e''',
        1 ~> n d''',
        11 ~> sn e''',
        1 ~> n dflat''',
        12 ~> n c'''
      ]

    n :: Double -> Time -> Signal Double
    n frequency length =
      nadsr length $
      constSpeedup frequency wave

    sn frequency =
      ns $ \ len ->
        take (len * 1.5) $
        ramp (pitch (-0.7) frequency) frequency 0.3 |> constant frequency

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
  raster (l 1) $
    8 .> partA :
    8 .> partA :
    14 .> partB :
    8 .> partA :
    []

partA =
  raster (l 1) $ fmap (1 .>) $
    chord [f'', c''', e''', a'''] :
    chord [f'', c''', e''', a'''] :
    chord [bflat', d''', f''', a'''] :
    chord [bflat', d''', f''', a'''] :
    chord [f'', c''', e''', a'''] :
    chord [f'', c''', e''', a'''] :
    chord [c'', bflat'', e''', g'''] :
    chord [c'', bflat'', e''', g'''] :
    []

partB =
  raster (l 1) $ fmap (1 .>) $
    chord [bflat', a'', d''', f'''] :
    chord [b', aflat'', d''', f'''] :
    chord [c'', a'', e''', f'''] :
    chord [c'', a'', eflat''', f'''] :
    chord [bflat', a'', d''', f'''] :
    chord [bflat', aflat'', dflat''', f'''] :
    chord [a', g'', c''', e'''] :
    chord [d'', fsharp'', c''', e'''] :
    chord [bflat', a'', d''', f'''] :
    chord [bflat', aflat'', dflat''', f'''] :
    chord [a', g'', c''', e'''] :
    chord [d'', fsharp'', c''', e'''] :
    chord [g', f'', bflat'', d'''] :
    chord [c'', e'', bflat'', d'''] :
    []

chord frequencies =
  fanOut (adsr (l 1 * 1.1) (Adsr 0.01 0.2 0.7 1) . note) frequencies

note frequency = constSpeedup frequency $ harmonics [1, 0.5, 0.9, 0.3, 0.6]

phaser :: Signal Double -> Signal Double
phaser = onFinite inner
  where
    onFinite :: (Signal Double -> Signal Double) -> (Signal Double -> Signal Double)
    onFinite onInfinite signal = case end signal of
      Nothing -> onInfinite signal
      Just end ->
        take end (onInfinite (signal |> constant 0))

    phaseSignal =
      fmap (project (-1, 1) (1 - deviation, 1 + deviation)) $
      constSpeedup (fromTime (1 / frequency)) $
      sine

    inner signal =
      fmap (/ (1 + wetness)) $
        signal +++
        fmap (* wetness) (speedup phaseSignal signal)

    wetness = 1

    deviation = 0.001

    frequency = l 1 / 2

drums :: Signal Double
drums =
  raster (l 1 / 4) $ fmap (1 ~>) $
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
    []

  where
    kick _len =
      fmap (* 1.3) $
      fmap (compress 0.92) $
      adsr len (Adsr 0.01 0.02 0 0) $
      speedup (ramp 60 50 len) sine

    len = 0.05

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
