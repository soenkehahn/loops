import Signal
import Signal.Core
import Signal.Notes
import Signal.Snippet
import Prelude hiding (take)

main :: IO ()
main = printSamples song

song :: Signal Double
song = mix [fmap (* 0.6) bass, claps]

phaser :: Fractional a => Signal a -> Signal a
phaser signal =
  cutoff $
    fmap (/ 2) $
      speedup (fmap (\s -> 1 + s * 0.01) sine) (signal |> constant 0)
        +++ signal
  where
    cutoff =
      case end signal of
        Infinite -> id
        Finite end -> flip take end

claps :: Signal Double
claps =
  flip divide 4 $
    1 .> empty :
    1 .> clap d''' :
    1 .> empty :
    1 .> clap c''' :
    1 .> empty :
    1 .> clap f''' :
    0.5 .> empty :
    0.5 .> clap c''' :
    1 .> clap d''' :
    []

clap :: Double -> Signal Double
clap pitch =
  mix $
    map (\delay -> silence (delay * Time (1 / pitch)) |> r) [0 .. 3]
  where
    r = ramp 1 0 0.01

bass :: Signal Double
bass =
  phaser $
    flip divide 4 $
      3 ~> n d'' :
      1 ~> n a' :
      0.5 ~> n c'' :
      3.5 ~> n d'' :
      3 ~> n d'' :
      1 ~> n a' :
      0.5 ~> n c'' :
      3.5 ~> n d'' :
      []

n :: Double -> Time -> Signal Double
n pitch time =
  adsr time (Adsr 0.01 0.1 0 0)
    /\ constSpeedup pitch rect
