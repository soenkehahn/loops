{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}

module LeeLoo where

import Signal
import Signal.Snippet
import Signal.Utils
import Signal.Notes
import Prelude ()
import qualified Prelude
import Data.List (foldl')

l :: Time -> Time
l n = n * 4.5

leeloo :: Signal Double
leeloo =
  focus 0 (l 4) $
  chords +++
  melody +++
  tiktok +++
  empty

melody =
  fmap (* 0.1) $
  raster (l 1 / 12) [
    2 ~> sn a''',
    4 ~> n c'''',
    2 ~> sn d'''',
    2 ~> n c'''',
    11 ~> \ t -> ns (divide [
      2 ~> \ t -> ramp t a''' aflat''',
      2 ~> \ t -> ramp t aflat''' a''',
      7 ~> \ _t -> constant a'''
     ] t) t,
    3 ~> evenly [sn (pitch (- 0.4) aflat'''), n g''', n f'''],
    12 ~> n d'''
  ]

  where

    n :: Double -> Time -> Signal Double
    n frequency length =
      nadsr length $
      constSpeedup frequency rect

    sn frequency length =
      nadsr length $
      speedup (ramp 0.3 (pitch (-1) frequency) frequency |> constant frequency) rect

    ns :: Signal Double -> Time -> Signal Double
    ns frequency length =
      nadsr length $
      speedup frequency rect

    nadsr length = adsr (length + 0.3) (Adsr 0.1 0.2 0.7 0.3)


chords =
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

note frequency = harmonics [1, 0.5, 0.9, 0.3, 0.6] frequency

harmonics :: [Double] -> Double -> Signal Double
harmonics weights frequency =
  foldl' (\ acc (natural, weight) -> acc +++ tone natural weight) empty $
    Prelude.zip [1 ..] weights
  where
    tone natural weight =
      fmap (* weight) (constSpeedup (frequency * natural) sine)

tiktok :: Signal Double
tiktok =
  fmap (* 0.2) $
  cycle $
  fill (l (1 / 12)) $ take 0.001 (constant 1)
