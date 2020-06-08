{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Song (main) where

https://www.desmos.com/calculator/q9oxbzkrje

import Signal
import Signal.Core
import Signal.Utils
import Prelude ()
import System.IO

main :: IO (Signal Double)
main = do
  hPutStrLn stderr (show (map nLogN [0, 0.1 .. 3]))
  return song

song :: Signal Double
song =
  fmap (* 0.03) $
  takeWithOfframp $
  -- foldl' (|>) empty ns
  flip take (4.2 * factor) $
  mix $ map (uncurry fromFunction) [
    (nLogN, [1, 0.3]),
    (parabola, [1, 0, 0.2]),
    (\ (Time t) -> 1 + t * 0.6, [1, 0, 0.1, 0, 0.12, 0, 0.07])
  ]

factor = 1.75

fromFunction :: (Time -> Double) -> [Double] -> Signal Double
fromFunction f weights =
  speedup (simpleSignal (\ time -> f (time / factor))) $
  constSpeedup 200 (harmonics weights)

nLogN :: Time -> Double
nLogN (Time t) =
  let result = 1 + t * log t
  in if isNaN result then 1 else result

parabola (Time t) = 4 - (t - 2) ^ (2 :: Int) + 1

takeWithOfframp :: Signal Double -> Signal Double
takeWithOfframp signal = case end signal of
  Finite end ->
    (onramp |> take (constant 1) (end - offrampLength - onrampLength) |> offramp)
    /\ signal
  Infinite -> (onramp |> constant 1) /\ signal
  where
    offramp = ramp 1 0 offrampLength
    offrampLength = 1
    onramp = ramp 0 1 onrampLength
    onrampLength = 0.15
