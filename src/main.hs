
import Signal
import Prelude hiding (take)
import Data.Function

main :: IO ()
main = do
  putStr $ unlines $ map show $ toList (1 / 44100) $ take 1 loop

loop :: Signal Double
loop =
  note 200 |> note 400

note :: Double -> Signal Double
note frequency = take 0.2 $ speedup (constant frequency) $ fmap sin phase
