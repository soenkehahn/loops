{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Loop where

import Signal
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
  -- take bar $
    song |>
    -- (ramp (bar * 4) 1 0 /\ song) |>
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
    part 1 |>
    part (4 / 3) |>
    empty
  where
    part base =
      arp base [200, 400, 300, 250] |>
      arp base [200, 400, 300, 240] |>
      arp base [200, 400, 300, 250] |>
      arp base [200, 400, 300, 240] |>
      empty

arp base frequencies =
  repeat 4 (foldl' (\ acc frequency -> acc |> note base frequency) empty frequencies)

note :: Double -> Double -> Signal Double
note base frequency = adsr (beat / 4) (Adsr 0.001 0.1 0.3 0.05) $ speedup (constant (base * frequency)) $ fmap sin phase

snares =
  echo 0.1 0.2 $
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

inBars length signals = foldl' (|>) empty $ map (fill length) signals

snare =
  random (-1, 1)
    & take 0.06
    & fmap (* 0.3)

bass base =
  shift (- 0.02) $
  inBars bar $
    fill 3 (n 50) |> n 25 :
    n 50 :
    fill 3 (n 50) |> n 25 :
    n 50 |> n 25 |> n 50 :
    []
  where
    n frequency =
      adsr (beat / 4) (Adsr 0.01 ((beat / 4) - 0.01) 0 0) $
      speedup (constant (base * frequency) +++ (take (beat / 8) (constant 0) |> ramp (beat / 8) 0 (-7))) $
      fmap (clip (-1, 1) . (* 10)) $
      sine
