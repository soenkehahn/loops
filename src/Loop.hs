{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Loop where

import Signal
import Bars
import Transformations
import Prelude ()
import Data.Function
import Data.List (foldl')
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  BS.putStr $ toByteString (1 / 44100) $ take 100 loop

bar = 3.2

beat = bar / 4

loop :: Signal Double
loop =
  fmap (* 0.1) $
  take (bar * 8) $
    song |>
    (ramp (bar * 4) 1 0 /\ song) |>
    silence 5 |>
    empty
  where
    song =
      band +++
      fmap (* 0.02) melody

melody =
  inBars (bar * 4) $
    n 410 480 |> n 420 400 :
    n 410 480 |> n 420 400 :
    []
  where
    n start frequency = adsr 6.0 (Adsr 0.1 1 0.7 2.3) $
      speedup (ramp 0.7 start frequency |> constant frequency) rect

band =
  take (bar * 8) $
  fmap (* 0.3) $
    arps +++ snares +++ (bass 1 |> bass (4 / 3))

arps =
  echo 0.31 0.5 $
    foldl' (\ acc part -> acc |> arp part) empty (bars [2, 4, 4])
  where
    arp part =
      foldl' (\ acc frequency -> acc |> note frequency) empty $ fmap (* 200) $
        case part of
          [0, 0, _] -> [1, 2, 1.5, 1.25]
          [0, 1, _] -> [1, 2, 1.5, 1.2]
          [0, 2, _] -> [1, 2, 1.5, 1.25]
          [0, 3, _] -> [1, 2, 1.5, 1.2]
          [1, 0, _] -> fmap (* (4 / 3)) [1, 2, 1.5, 1.25]
          [1, 1, 2] -> fmap (* (4 / 3)) [1, 1.2, 1.5, 1.5 * (9 / 8)]
          [1, 1, 3] -> fmap (* (4 / 3)) [1.5 * (5 / 4), 1.5 * (9 / 8), 1.5, 1.2]
          [1, 1, _] -> fmap (* (4 / 3)) [1, 2, 1.5, 1.2]
          [1, 2, _] -> fmap (* (4 / 3)) [1, 2, 1.5, 1.25]
          [1, 3, 2] -> fmap (* (4 / 3)) [1, 1.2, 1.5, 1.5 * (9 / 8)]
          [1, 3, 3] -> fmap (* (4 / 3)) [1.5 * (5 / 4), 2 * (9 / 8), 1.5, 1.2]
          [1, 3, _] -> fmap (* (4 / 3)) [1, 2, 1.5, 1.2]

    note :: Double -> Signal Double
    note frequency =
      adsr (beat / 4) (Adsr 0.001 0.1 0.3 0.05) $
        speedup (constant frequency) $ fmap sin phase

inBars length signals = foldl' (|>) empty $ map (fill length) signals

snares =
  echo 0.105 0.05 $
  inBars (2 * beat) $
    silence 0.8 |> snare :
    silence 0.8 |> snare :
    silence 0.8 |> snare |> silence 0.5 |> snare :
    silence 0.8 |> snare :
    silence 0.8 |> snare :
    silence 0.8 |> fill 0.4 snare |> snare :
    silence 0.8 |> fill 0.6 snare |> snare :
    silence 0.6 |> fill 0.6 snare |> snare :
    silence 0.8 |> snare :
    silence 0.8 |> snare :
    silence 0.8 |> snare |> silence 0.5 |> snare :
    silence 0.8 |> snare :
    silence 0.8 |> snare :
    silence 0.8 |> fill 0.4 snare |> snare :
    silence 0.8 |> fill 0.6 snare |> snare :
    silence 0.6 |> fill 0.6 snare |> snare :
    []

snare =
  random (-1, 1)
    & adsr 0.06 (Adsr 0.01 0.05 0.2 0.01)
    & fmap (* 0.3)

bass base =
  skip 0.02 $
  inBars bar $
    fill 3 (n 50) |> n 25 :
    n 50 :
    fill 3 (n 50) |> n 25 :
    n 50 |> n 25 |> n 50 :
    []
  where
    n frequency =
      adsr 0.2 (Adsr 0.01 0.06 0.5 0.01) $
      speedup (constant (base * frequency) +++ (take (beat / 8) (constant 0) |> ramp (beat / 8) 0 (-7))) $
      fmap (clip (-1, 1) . (* 7)) $
      saw
