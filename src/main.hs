
import Signal
import Prelude hiding (take)
import Data.Function

main :: IO ()
main = do
  putStr $ unlines $ map show $ toList (1 / 44100) $ take 1 loop

loop :: Signal Double
loop = do
  let lfo = fmap sin phase
        & fmap (project (-1, 1) (430, 450))
  speedup lfo $ fmap sin phase
