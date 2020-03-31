{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}

module LeeLoo where

import Signal
import Signal.Snippet
import Prelude hiding (zip, take)
import qualified Prelude
import Data.List hiding (take)

leeloo :: Signal Double
leeloo =
  fmap (* 0.03) $
  fill 2 $
  runSnippet $
    mempty
    <> 0 |->
      fanOut (\ f -> adsr 1 (Adsr 0.01 0.2 0.7 0.01) (note f)) [e', bflat', d'', g'']

note frequency = harmonics [1, 0.5, 0.9, 0.3, 0.8] frequency

harmonics :: [Double] -> Double -> Signal Double
harmonics weights frequency =
  foldl' (\ acc (natural, weight) -> acc +++ tone natural weight) empty $
    Prelude.zip [1 ..] weights
  where
    tone natural weight =
      fmap (* weight) (constSpeedup (frequency * natural) sine)
