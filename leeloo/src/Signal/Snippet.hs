{-# LANGUAGE ExistentialQuantification #-}
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
  _signalVectorConfiguration,
) where

import Signal
import Signal.Epsilon
import Control.Monad.ST
import Data.List (foldl')
import Data.Vector (Vector, (!), fromList)

infix 7 |->
(|->) :: Num a => Time -> Signal a -> Signal a
time |-> signal = silence time |> signal

data Part a = Part {
  weight :: Double,
  signal :: Time -> Signal a
}

(~>) :: Double -> (Time -> Signal a) -> Part a
(~>) = Part

(.>) :: Double -> Signal a -> Part a
weight .> signal = Part weight (const signal)

evenly :: Num a => [Time -> Signal a] -> Time -> Signal a
evenly parts = divide $ map (Part 1) parts

raster :: Num a => Time -> [Part a] -> Signal a
raster unit parts = divide parts (unit * Time (sum (map weight parts)))

divide :: Num a => [Part a] -> Time -> Signal a
divide [] length = silence length
divide parts length =
  let snippets = mkSnippets parts length
  in Signal (partsLength snippets length) $ do
    signalVector <- mkSignalVector (_signalVectorConfiguration parts length) snippets
    return $ \ time -> do
      runSignalVector signalVector time

data PreSnippet a
  = PreSnippet {
    preSnippetStart :: Time,
    preSnippetSignal :: Signal a
  }

mkSnippets :: [Part a] -> Time -> [PreSnippet a]
mkSnippets parts length =
  snd $ foldl' inner (0, []) parts
  where
    timeUnit = length / Time (sum (map weight parts))
    inner (time, acc) (Part weight signal) =
      let timeInGrid = Time weight * timeUnit
      in (time + timeInGrid, PreSnippet time (signal timeInGrid) : acc)

partsLength :: [PreSnippet a] -> Time -> Length
partsLength snippets length =
  maxLength (Finite length) maxSnippetLength
  where
    snippetLength :: PreSnippet a -> Length
    snippetLength snippet =
      mapLength (+ preSnippetStart snippet) (signalLength (preSnippetSignal snippet))
    maxSnippetLength = foldl' maxLength (Finite 0) (map snippetLength snippets)

_signalVectorConfiguration :: [Part a] -> Time -> (Int, Time)
_signalVectorConfiguration parts length =
  (vectorLength, tailStart)
  where
    timeUnit = length / Time (sum (map weight parts))
    vectorLength = ceiling $ fromTime (length / timeUnit)
    tailStart = timeUnit * Time (fromIntegral (ceiling (sum (map weight parts)) :: Int))

data SignalVector s a
  = SignalVector {
    _vector :: Vector [Snippet s a],
    _tailStart :: Time,
    _tail :: [Snippet s a]
  }

runSignalVector :: Num a => SignalVector s a -> Time -> ST s a
runSignalVector (SignalVector vector tailStart tail) time =
  runSnippets snippets time
  where
    snippets = if time `lt` tailStart
      then
        let index = floor (fromTime (time / tailStart) * fromIntegral (length vector))
        in vector ! index
      else tail

mkSignalVector :: forall a s . Num a =>
  (Int, Time) -> [PreSnippet a] -> ST s (SignalVector s a)
mkSignalVector (vectorLength, tailStart) snippets = do
  initializedSnippets <- mapM initializeSnippet snippets
  return $
    let step = tailStart / fromIntegral vectorLength
        mkCell start =
          prune (start, Finite (start + tailStart / fromIntegral vectorLength)) initializedSnippets
        vector = Data.Vector.fromList $ map mkCell (init [0, step .. tailStart])
        tail = prune (tailStart, Infinite) initializedSnippets
    in SignalVector vector tailStart tail

data Snippet s a
  = Snippet {
    snippetStart :: Time,
    snippetLength :: Length,
    runSnippet :: Time -> ST s a
  }

initializeSnippet :: PreSnippet a -> ST s (Snippet s a)
initializeSnippet (PreSnippet start (Signal length initialize)) = do
  runSignal <- initialize
  return $ Snippet start length runSignal

runSnippets :: Num a => [Snippet s a] -> Time -> ST s a
runSnippets snippets time = case snippets of
  snippet : rest -> do
    sample <- if inRange snippet time
      then runSnippet snippet (time - snippetStart snippet)
      else return 0
    (sample +) <$> runSnippets rest time
  [] -> return 0

inRange :: Snippet s a -> Time -> Bool
inRange (Snippet start length _) time = case length of
  Infinite -> not (time `lt` start)
  Finite length ->
    not (time `lt` start) &&
    time `lt` (start + length)

prune :: (Time, Length) -> [Snippet s a] -> [Snippet s a]
prune range = filter (withinRange range)

withinRange :: (Time, Length) -> Snippet s a -> Bool
withinRange (lowerEnd, upperEnd) snippet =
  not tooEarly && not tooLate
  where
    tooEarly = case snippetLength snippet of
      Infinite -> False
      Finite length -> (snippetStart snippet + length) `lt` lowerEnd
    tooLate = case upperEnd of
      Infinite -> False
      Finite upperEnd -> upperEnd `lt` snippetStart snippet
