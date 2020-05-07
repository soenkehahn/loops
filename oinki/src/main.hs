import Signal.Core
import qualified Song

main :: IO ()
main = printSamples =<< Song.main
