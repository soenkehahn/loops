{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}

module LeeLoo where

import Signal
import Signal.Snippet
import Signal.Utils
import Signal.Notes
import Prelude hiding (zip, take)
import qualified Prelude
import Data.List hiding (take)

leeloo :: Signal Double
leeloo =
  -- focus 0 (l 38) $
  fmap (* 0.03) $
  runSnippet $
    mempty
    <> l 0 |-> partA
    <> l 8 |-> partA
    <> l 16 |-> partB
    <> l 30 |-> partA

partA =
  runSnippet $
    mempty
    <> l 0 |->
      chord [f'', c''', e''', a''']
    <> l 1 |->
      chord [f'', c''', e''', a''']
    <> l 2 |->
      chord [f'', c''', e''', a''']
    <> l 3 |->
      chord [bflat', d''', f''', a''']
    <> l 4 |->
      chord [f'', c''', e''', a''']
    <> l 5 |->
      chord [c'', bflat'', e''', g''']
    <> l 6 |->
      chord [f'', c''', e''', a''']
    <> l 7 |->
      chord [c'', bflat'', e''', g''']

l n = 3 * n

partB = runSnippet $
  mempty
  <> l 0 |-> chord [bflat', a'', d''', f''']
  <> l 1 |-> chord [b', aflat'', d''', f''']
  <> l 2 |-> chord [c'', a'', e''', f''']
  <> l 3 |-> chord [c'', a'', eflat''', f''']
  <> l 4 |-> chord [bflat', a'', d''', f''']
  <> l 5 |-> chord [bflat', aflat'', dflat''', f''']
  <> l 6 |-> chord [a', g'', c''', e''']
  <> l 7 |-> chord [d'', fsharp'', c''', e''']
  <> l 8 |-> chord [bflat', a'', d''', f''']
  <> l 9 |-> chord [bflat', aflat'', dflat''', f''']
  <> l 10 |-> chord [a', g'', c''', e''']
  <> l 11 |-> chord [d'', fsharp'', c''', e''']
  <> l 12 |-> chord [g', f'', bflat'', d''']
  <> l 13 |-> chord [c'', e'', bflat'', d''']

chord frequencies =
  fanOut (adsr 3.3 (Adsr 0.01 0.2 0.7 1) . note) frequencies

note frequency = harmonics [1, 0.5, 0.9, 0.3, 0.6] frequency

harmonics :: [Double] -> Double -> Signal Double
harmonics weights frequency =
  foldl' (\ acc (natural, weight) -> acc +++ tone natural weight) empty $
    Prelude.zip [1 ..] weights
  where
    tone natural weight =
      fmap (* weight) (constSpeedup (frequency * natural) sine)
