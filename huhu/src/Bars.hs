{-# LANGUAGE ScopedTypeVariables #-}

module Bars (
  module Bars,
  Generic,
  AllValues,
  allValues,
) where

import Signal
import Epsilon
import AllValues
import Prelude hiding (take)

bars :: [Int] -> [[Int]]
bars limits = case limits of
  limit : r -> do
    first <- [0 .. pred limit]
    rest <- bars r
    return (first : rest)
  [] -> [[]]

orchestrate :: forall a part . (PartLength part, Num a) => (part -> Signal a) -> Signal a
orchestrate signal = inner Nothing (allValues :: [part])
  where
    inner overlap parts = case parts of
      part : r -> maybe id (+++) overlap $
        let nextSignal = signal part
            thisPart = fill (partLength part) nextSignal
            nextPart = skip (partLength part) nextSignal
        in thisPart |> case end nextSignal of
          Just end -> case Epsilon.compare end (partLength part) of
            EQ -> inner Nothing r
            LT -> inner Nothing r
            GT -> inner (Just nextPart) r
          Nothing -> inner (Just nextPart) r
      [] -> empty

class (Eq part, AllValues part) => PartLength part where
  partLength :: part -> Time
