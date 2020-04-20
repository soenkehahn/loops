{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Snippet where

import Signal
import Signal.Epsilon
import Control.Monad
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
    signalVector <- mkSignalVector snippets
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

partsLength :: [PreSnippet a] -> Time -> Maybe Time
partsLength snippets length = do
  snippetTimes <- forM snippets $ \ snippet -> do
    length <- end $ preSnippetSignal snippet
    return (preSnippetStart snippet + length)
  return $ maxTime length (maximumTime snippetTimes)

data SignalVector s a
  = SignalVector {
    vector :: Vector [Snippet s a],
    tailStart :: Time,
    tail :: [Snippet s a]
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

mkSignalVector :: forall a s . Num a => [PreSnippet a] -> ST s (SignalVector s a)
mkSignalVector snippets = do
  initializedSnippets <- mapM initializeSnippet snippets
  return $
    let tailStart = 25
        vectorLength = 500
        step = tailStart / vectorLength
        mkCell start =
          prune (start, Just (start + tailStart / vectorLength)) initializedSnippets
        vector = Data.Vector.fromList $ map mkCell (init [0, step .. tailStart])
        tail = prune (tailStart, Nothing) initializedSnippets
    in SignalVector vector tailStart tail

data Snippet s a
  = Snippet {
    snippetStart :: Time,
    snippetLength :: Maybe Time,
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
  Nothing -> not (time `lt` start)
  Just length ->
    not (time `lt` start) &&
    time `lt` (start + length)

prune :: (Time, Maybe Time) -> [Snippet s a] -> [Snippet s a]
prune range = filter (withinRange range)

withinRange :: (Time, Maybe Time) -> Snippet s a -> Bool
withinRange (lowerEnd, upperEnd) snippet =
  maybe True (\ length -> lowerEnd `lt` (snippetStart snippet + length)) (snippetLength snippet) &&
  maybe True (\ upperEnd -> snippetStart snippet `lt` upperEnd) upperEnd
