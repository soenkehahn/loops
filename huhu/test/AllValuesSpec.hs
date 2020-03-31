{-# LANGUAGE DeriveGeneric #-}

module AllValuesSpec where

import Test.Hspec
import AllValues

data SimpleEnum = A | B | C
  deriving (Show, Eq, Generic)

instance AllValues SimpleEnum

data Pair a b = Pair a b
  deriving (Show, Eq, Generic)

instance (AllValues a, AllValues b) => AllValues (Pair a b)

data Unit = Unit
  deriving (Show, Eq, Generic)

instance AllValues Unit

data ADT
  = Foo SimpleEnum
  | Bar (Pair Unit Bool) SimpleEnum
  deriving (Show, Eq, Generic)

instance AllValues ADT

spec :: Spec
spec = do
  describe "allValues" $ do
    it "enumerates all values of an enum type" $ do
      allValues `shouldBe` [A, B, C]

    it "enumerates all values of a product type, last field increasing first" $ do
      let expected =
            Pair A A :
            Pair A B :
            Pair A C :
            Pair B A :
            Pair B B :
            Pair B C :
            Pair C A :
            Pair C B :
            Pair C C :
            []
      allValues `shouldBe` expected

    it "enumerates unit types" $ do
      allValues `shouldBe` [Unit]

    it "enumerates full ADTs" $ do
      let expected =
            Foo A :
            Foo B :
            Foo C :
            Bar (Pair Unit False) A :
            Bar (Pair Unit False) B :
            Bar (Pair Unit False) C :
            Bar (Pair Unit True) A :
            Bar (Pair Unit True) B :
            Bar (Pair Unit True) C :
            []
      allValues `shouldBe` expected
