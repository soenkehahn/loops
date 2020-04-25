import Criterion.Main
import Prelude ()
import Signal
import Signal.Utils

main :: IO ()
main = do
  defaultMain $
    bench "unmemoized harmonics" (nf (getSample unmemoized) 5.5) :
    bench "memoized harmonics" (nf (getSample memoized) 5.5) :
    []

unmemoized :: Signal Double
unmemoized =
  constSpeedup 200 $ unmemoizedHarmonics [1, 0.9 .. 0]

memoized :: Signal Double
memoized =
  constSpeedup 200 $ harmonics [1, 0.9 .. 0]
