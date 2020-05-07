{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Song (main) where

import Signal
import Signal.Core
import Prelude ()

main :: IO (Signal Double)
main = do
  return song

song :: Signal Double
song =
  flip take 3 $
  constSpeedup 200 $
  sine
