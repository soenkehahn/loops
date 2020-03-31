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
  -- focus 0 2 $
  fmap (* 0.03) $
  runSnippet $
    mempty
    <> 0 |->
      chord [e'', bflat'', d''', g''']
    <> 2 |->
      chord [a', g'', csharp''', f''']
    <> 4 |->
      chord [bflat', a'', d''', g''']
    <> 6 |->
      chord [b', g'', d''', f''']
    <> 8 |->
      chord [c'', bflat'', e''', g''']
    <> 10 |->
      chord [csharp'', a'', e''', g''']
    <> 12 |->
      chord [d'', bflat'', f''', a''']
    <> 14 |->
      chord [b', g'', d''', f''']
    <> 16 |->
      chord [c'', bflat'', e''', g''']
    <> 18 |->
      chord [csharp'', a'', e''', g''']
    <> 20 |->
      chord [d'', bflat'', f''', a''']
    <> 22 |->
      chord [b', g'', d''', f''']
    <> 24 |->
      chord [c'', bflat'', e''', g''']
    <> 26 |->
      chord [csharp'', a'', e''', g''']
    <> 28 |->
      chord [d'', b'', f''', a''']
    <> 30 |->
      chord [b', aflat'', d''', f''']

chord frequencies =
  fanOut (adsr 2.3 (Adsr 0.01 0.2 0.7 1) . note) frequencies

note frequency = harmonics [1, 0.5, 0.9, 0.3, 0.8] frequency

harmonics :: [Double] -> Double -> Signal Double
harmonics weights frequency =
  foldl' (\ acc (natural, weight) -> acc +++ tone natural weight) empty $
    Prelude.zip [1 ..] weights
  where
    tone natural weight =
      fmap (* weight) (constSpeedup (frequency * natural) sine)
