{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Snippet (
  (|->),
  divide,
  evenly,
  raster,
  (~>),
  (.>),

  -- exported for testing
  Part(..),
  mkSnippets_,
  mkVector_,
  computeIndex_,
) where

import Debug.Trace
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.List (foldl', maximumBy)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import Signal
import Signal.Epsilon

infix 7 |->
(|->) :: Num a => Time -> Signal a -> Signal a
time |-> signal = silence time |> signal

divide :: Num a => [Part a] -> Time -> Signal a
divide [] len = silence len
divide parts len = Signal (Just len) $ do
  let snippets = mkSnippets_ parts len
  signalVector <- mkVector_ (vectorLength_ parts len) snippets
  let vectorLength = Vec.length signalVector
  return $ \ time -> do
    let index = computeIndex_ len vectorLength time
    when (index < 0) $
      error $ show (len, vectorLength, time, index)
    (signalVector ! index) time

computeIndex_ :: Time -> Int -> Time -> Int
computeIndex_ len vectorLength time =
  let candidate = fromTime (time / len) * fromIntegral vectorLength
  in Signal.Epsilon.floor candidate

mkSnippets_ :: [Part a] -> Time -> [(Time, Signal a)]
mkSnippets_ parts len =
  reverse $ snd $ foldl' inner (0, []) parts
  where
    timeUnit = len / Time (fromRational (sum (map weight parts)))
    inner :: (Time, [(Time, Signal a)]) -> Part a -> (Time, [(Time, Signal a)])
    inner (time, acc) (Part weight signal) =
      let len = Time (fromRational weight) * timeUnit
      in (time + len, (time, signal len) : acc)

-- fixme: don't use fromJust
-- fixme: don't iterate through lists for every sample...

mkVector_ :: Num a => Int -> [(Time, Signal a)] -> ST s (Vector (Time -> ST s a))
mkVector_ vectorLength snippets = do
  let minTime = 0
      maxTime = maximumBy Signal.Epsilon.compare $ map (\ (start, signal) -> start + (fromJust $ end signal)) snippets
      cellLength = (maxTime - minTime) / fromIntegral vectorLength
  initializedSnippets :: [(Time, Signal a, Time -> ST s a)] <-
    forM snippets $ \ (start, signal) -> do
      i <- initialize signal
      return (start, signal, i)
  return $ Vec.generate vectorLength $ \ index time -> do
    let start = fromIntegral index * cellLength
        end = (fromIntegral index + 1) * cellLength
    samples <- forM (filter (overlapping (start, end)) initializedSnippets) $ \ (start, signal, runSignal) -> do
      let t = time - start
      case Signal.end signal of
        Just end | not (t `lt` end && not (time `lt` start)) -> do
          -- trace "muting signal" (return ())
          return 0
        _ -> runSignal t
    return $ sum samples

overlapping :: (Time, Time) -> (Time, Signal a, b) -> Bool
overlapping (cellStart, cellEnd) (signalStart, signal, _) =
  (signalStart `lt` cellEnd &&
   cellStart `lt` (signalStart + fromJust (end signal)))

vectorLength_ :: [Part a] -> Time -> Int
vectorLength_ _ _ = 10

data Part a = Part {
  weight :: Rational,
  signal :: Time -> Signal a
}

(~>) :: Rational -> (Time -> Signal a) -> Part a
(~>) = Part

(.>) :: Rational -> Signal a -> Part a
weight .> signal = Part weight (const signal)

evenly :: Num a => [Time -> Signal a] -> Time -> Signal a
evenly parts = divide $ map (Part 1) parts

raster :: Num a => Time -> [Part a] -> Signal a
raster unit parts = divide parts (unit * Time (fromRational (sum (map weight parts))))
