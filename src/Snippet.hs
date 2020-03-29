module Snippet where

import Signal

data Snippet = Snippet (Signal Double)

instance Semigroup Snippet where
  Snippet a <> Snippet b = Snippet (a +++ b)

instance Monoid Snippet where
  mempty = Snippet empty

runSnippet :: Snippet -> Signal Double
runSnippet (Snippet signal) = signal

infix 7 |->
(|->) :: Time -> Signal Double -> Snippet
time |-> signal = Snippet (silence time |> signal)
