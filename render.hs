#!/usr/bin/env stack
{- stack script --resolver lts-14.3 -}

import Control.Monad.Parallel
import Development.Shake
import System.FilePath

songs =
  "huhu" :
  "leeloo" :
  "vommke" :
  "merhannigan" :
  "olmfeld" :
  []

main = do
  unit $ cmd "rm -rf album"
  unit $ cmd "mkdir album"
  forM_ (zip [1 ..] songs) $ \ (n, directory) -> do
    let outputFile = "album" </> (show n ++ "_" ++ directory ++ ".wav")
    unit $ cmd (Cwd directory)
      "looper ./run.sh --normalize --render"
      (".." </> outputFile)
    putStrLn $ "done rendering " ++ outputFile
