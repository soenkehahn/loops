module Signal.Snippet where

import Signal

infix 7 |->
(|->) :: Num a => Time -> Signal a -> Signal a
time |-> signal = silence time |> signal

divide :: Num a => [Part a] -> Time -> Signal a
divide [] len = silence len
divide parts len = inner parts
  where
    timeUnit = len / Time (sum (map weight parts))
    inner parts = case parts of
      Part weight part : rest ->
        let len = (timeUnit * Time weight)
        in part len +++
           len |-> inner rest
      [] -> empty

data Part a = Part {
  weight :: Double,
  signal :: Time -> Signal a
}

(~>) :: Double -> (Time -> Signal a) -> Part a
(~>) = Part

evenly :: Num a => [Time -> Signal a] -> Time -> Signal a
evenly parts = divide $ map (Part 1) parts

raster :: Num a => Time -> [Part a] -> Signal a
raster unit parts = divide parts (unit * Time (sum (map weight parts)))

ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap f x = fmap (fmap f) x
