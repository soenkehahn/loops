{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module AllValues (
  module AllValues,
  Generic,
) where

import Generics.Eot

class AllValues a where
  allValues :: [a]
  default allValues :: (HasEot a, AllValues (Eot a)) => [a]
  allValues = map fromEot allValues

instance forall a b . (AllValues a, AllValues b) => AllValues (Either a b) where
  allValues =
    map Left (allValues :: [a]) ++
    map Right (allValues :: [b])

instance AllValues () where
  allValues = [()]

instance AllValues Void where
  allValues = []

instance (AllValues a, AllValues b) => AllValues (a, b) where
  allValues = do
    a <- allValues
    b <- allValues
    return (a, b)

instance AllValues Bool
