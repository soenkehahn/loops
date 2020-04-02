#!/usr/bin/env stack
{- stack script --resolver lts-14.3 -}

import Development.Shake
import Data.Foldable
import System.FilePath

songs =
  "huhu" :
  "leeloo" :
  []

main = do
  unit $ cmd "rm -rf album"
  unit $ cmd "mkdir album"
  forM_ (zip [1 ..] songs) $ \ (n, directory) -> do
    let outputFile = "album" </> (show n ++ "_" ++ directory ++ ".wav")
    putStrLn $ "rendering " ++ outputFile ++ "..."
    unit $ cmd (Cwd directory) "looper ./run.sh --render" (".." </> outputFile)
