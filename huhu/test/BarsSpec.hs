{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module BarsSpec where

import Test.Hspec
import Bars
import Prelude hiding (take)
import Signal
import Test.Utils

spec :: Spec
spec = do
  describe "bars" $ do
    it "returns a list with bars up to the given maximum" $ do
      bars [4] `shouldBe` [[0], [1], [2], [3]]

    it "works for different limits" $ do
      bars [3] `shouldBe` [[0], [1], [2]]

    it "allows to generate beats" $ do
      let expected =
            [0, 0] :
            [0, 1] :
            [0, 2] :
            [0, 3] :
            [1, 0] :
            [1, 1] :
            [1, 2] :
            [1, 3] :
            [2, 0] :
            [2, 1] :
            [2, 2] :
            [2, 3] :
            [3, 0] :
            [3, 1] :
            [3, 2] :
            [3, 3] :
            []
      bars [4, 4] `shouldBe` expected

    it "allows to generate parts, bars and beats" $ do
      let expected =
            [0, 0, 0] :
            [0, 0, 1] :
            [0, 1, 0] :
            [0, 1, 1] :
            [1, 0, 0] :
            [1, 0, 1] :
            [1, 1, 0] :
            [1, 1, 1] :
            []
      bars [2, 2, 2] `shouldBe` expected

  orchestrateSpec

data Part = A | B
  deriving (Show, Eq, Generic)

instance AllValues Part

instance PartLength Part where
  partLength = const 1

data DifferentLengths
  = Longer
  | Shorter
  deriving (Show, Eq, Generic)

instance AllValues DifferentLengths

instance PartLength DifferentLengths where
  partLength = \case
    Longer -> 2
    Shorter -> 1

data Nested
  = First Part
  | Second Part Part
  deriving (Show, Eq, Generic)

instance AllValues Nested

instance PartLength Nested where
  partLength = const 1

orchestrateSpec :: Spec
orchestrateSpec = do
  describe "orchestrate" $ do
    it "orchestrates signals based on the assigned length" $ do
      let signal :: Part -> Signal Integer
          signal = \case
            A -> take 1 $ constant 23
            B -> take 1 $ constant 42
      test 0.5 10 (orchestrate signal) [23, 23, 42, 42]

    it "allows parts with different lengths" $ do
      let signal :: DifferentLengths -> Signal Integer
          signal = \case
            Longer -> take 2 $ constant 23
            Shorter -> take 1 $ constant 42
      test 1 10 (orchestrate signal) [23, 23, 42]

    it "allows longer signals and makes them overlap" $ do
      let signal :: Part -> Signal Integer
          signal = \case
            A -> take 1.5 $ constant 23
            B -> take 1 $ constant 42
      test 0.5 10 (orchestrate signal) [23, 23, 65, 42]

    it "allows shorter signals by inserting silence" $ do
      let signal :: Part -> Signal Integer
          signal = \case
            A -> take 0.5 $ constant 23
            B -> take 1 $ constant 42
      test 0.5 10 (orchestrate signal) [23, 0, 42, 42]

    it "works with infinite signals" $ do
      let signal :: Part -> Signal Integer
          signal = \case
            A -> constant 23
            B -> take 1 $ constant 42
      test 0.5 3 (orchestrate signal) [23, 23, 65, 65, 23, 23]

    it "allows nested enum types" $ do
      let signal :: Nested -> Signal Integer
          signal part = take 1 $ case part of
            First A -> constant 1
            First B -> constant 2
            Second A A -> constant 3
            Second A B -> constant 4
            Second B A -> constant 5
            Second B B -> constant 6
      test 1 10 (orchestrate signal) [1, 2, 3, 4, 5, 6]
