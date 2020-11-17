{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -O0 #-}

module Utils where

import Control.Monad.Trans.State
import System.Random

randomInState :: (StdGen -> (a, StdGen)) -> State StdGen a
randomInState f = do
  gen <- get
  let (result, nextGen) = f gen
  put nextGen
  return result

choose :: [a] -> State StdGen a
choose list = do
  index <- randomInState $ randomR (0, length list - 1)
  return $ list !! index

weighted :: [(Int, a)] -> State StdGen a
weighted list = choose $ _weightedToUnweighted list

_weightedToUnweighted :: [(Int, a)] -> [a]
_weightedToUnweighted list = case list of
  (weight, element) : rest ->
    replicate weight element ++ _weightedToUnweighted rest
  [] -> []

range :: Random a => (a, a) -> State StdGen a
range bounds = randomInState $ randomR bounds

suchThat :: State StdGen a -> (a -> Bool) -> State StdGen a
suchThat action predicate = do
  candidate <- action
  if predicate candidate
    then return candidate
    else suchThat action predicate
