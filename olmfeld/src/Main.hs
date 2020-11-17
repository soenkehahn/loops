import Signal
import Signal.Core
import Signal.Notes
import Signal.Snippet
import Prelude hiding (take)

main :: IO ()
main = printSamples song

song :: Signal Double
song = mix [fmap (* 0.3) bass, claps]

claps :: Signal Double
claps =
  flip divide 4 $
    1 .> empty :
    1 .> clap d''' :
    1 .> empty :
    1 .> clap c''' :
    1 .> empty :
    1 .> clap f''' :
    1 .> empty :
    1 .> clap d''' :
    []

clap :: Double -> Signal Double
clap pitch =
  mix $
    map (\delay -> silence (delay * Time (1 / pitch)) |> r) [0 .. 3]
  where
    r = ramp 1 0 0.2

bass :: Signal Double
bass =
  divide
    [ 3 ~> n d'',
      1 ~> n a',
      0.5 ~> n c'',
      3.5 ~> n d'',
      3 ~> n d'',
      1 ~> n a',
      0.5 ~> n c'',
      3.5 ~> n d''
    ]
    4

n :: Double -> Time -> Signal Double
n pitch time =
  adsr time (Adsr 0.01 0.1 0 0)
    /\ constSpeedup pitch rect
