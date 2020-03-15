
import Signal
import Prelude ()
import Data.Function
import Data.List (foldl')

main :: IO ()
main = do
  putStr $ unlines $ map show $ toList (1 / 44100) $ take 100 loop

loop :: Signal Double
loop = arps +++ snares +++ bass

arps =
  arp [200, 400, 300, 250] |>
  arp [200, 400, 300, 240] |>
  arp [200, 400, 300, 250] |>
  arp [200, 400, 300, 240] |>
  empty

arp frequencies =
  repeat 4 (foldl' (\ acc frequency -> acc |> note frequency) empty frequencies)

note :: Double -> Signal Double
note frequency = take 0.2 $ speedup (constant frequency) $ fmap sin phase

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

bass =
  inBars 3.2 $
    fill 3 (n 50) |> n 25 :
    n 50 :
    fill 3 (n 50) |> n 25 :
    n 50 |> n 25 |> n 50 :
    []
  where
    n frequency = take 0.2 $ speedup (constant frequency) saw
