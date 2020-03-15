
import Signal
import Prelude ()
import Data.Function
import Data.List (foldl')

main :: IO ()
main = do
  putStr $ unlines $ map show $ toList (1 / 44100) $ take 100 loop

loop :: Signal Double
loop =
  -- take 1.6 $
    part 1 |>
    part (4 / 3) |>
    part 1 |>
    -- part (3 / 2) |>
    take 0.6 (part 1) |>
    empty
  where
    part base = arps base +++ snares +++ bass base

arps base =
  arp base [200, 400, 300, 250] |>
  arp base [200, 400, 300, 240] |>
  arp base [200, 400, 300, 250] |>
  arp base [200, 400, 300, 240] |>
  empty

arp base frequencies =
  repeat 4 (foldl' (\ acc frequency -> acc |> note base frequency) empty frequencies)

note :: Double -> Double -> Signal Double
note base frequency = take 0.2 $ speedup (constant (base * frequency)) $ fmap sin phase

snares =
  -- shift (- 0.03) $
  inBars 1.6 $
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
  inBars 3.2 $
    fill 3 (n 50) |> n 25 :
    n 50 :
    fill 3 (n 50) |> n 25 :
    n 50 |> n 25 |> n 50 :
    []
  where
    n frequency = take 0.2 $ speedup (constant (base * frequency) +++ ramp 0.3 0 (-20)) saw
