{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Signal.Epsilon where

import qualified Data.Vector as Vec

class EpsilonOrd a where
  compare :: a -> a -> Ordering

instance EpsilonOrd Char where
  compare = Prelude.compare

instance EpsilonOrd Integer where
  compare = Prelude.compare

instance EpsilonOrd Double where
  compare a b =
    if abs (a - b) < 0.000000001 then
      EQ
    else if a < b then
      LT
    else
      GT

lt :: EpsilonOrd a => a -> a -> Bool
lt a b = case Signal.Epsilon.compare a b of
  LT -> True
  _ -> False

class EpsilonEq a where
  (====) :: a -> a -> Bool
infix 4 ====

instance {-# OVERLAPPABLE #-} EpsilonOrd a => EpsilonEq a where
  a ==== b = case Signal.Epsilon.compare a b of
    EQ -> True
    _ -> False

instance EpsilonEq a => EpsilonEq [a] where
  as ==== bs = case (as, bs) of
    (a : ar, b : br) ->
      if a ==== b then ar ==== br else False
    ([], []) -> True
    _ -> False

instance EpsilonEq a => EpsilonEq (Vec.Vector a) where
  as ==== bs = Vec.toList as ==== Vec.toList bs

instance EpsilonEq a => EpsilonEq (Maybe a) where
  Nothing ==== Nothing = True
  Just a ==== Just b = a ==== b
  _ ==== _ = False

instance (EpsilonEq a, EpsilonEq b) => EpsilonEq (a, b) where
  (a, b) ==== (x, y) =
    a ==== x && b ==== y
