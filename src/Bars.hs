module Bars where

bars :: [Int] -> [[Int]]
bars limits = case limits of
  limit : r -> do
    first <- [0 .. pred limit]
    rest <- bars r
    return (first : rest)
  [] -> [[]]
