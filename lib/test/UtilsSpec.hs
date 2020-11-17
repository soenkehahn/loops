{-# LANGUAGE ScopedTypeVariables #-}

module UtilsSpec where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.State
import Data.HashSet (empty, insert)
import System.Random
import Test.Hspec
import Test.QuickCheck hiding (choose)
import Utils

spec :: Spec
spec = do
  describe "choose" $ do
    it "always picks an element of the given list" $ do
      property $ \gen (list :: [Int]) -> do
        not (null list)
          ==> evalState (choose list) (mkStdGen gen) `shouldSatisfy` (`elem` list)

    it "eventually picks all members of a list" $ do
      let list = [1 .. 50 :: Int]
      let loop set = do
            when (length set < length list) $ do
              new <- choose list
              loop (insert new set)
      _ <- evaluate $ runState (loop empty) (mkStdGen 42)
      return ()

  describe "_weightedToUnweighted" $ do
    it "repeats an element once for weight 1" $ do
      _weightedToUnweighted [(1, 'a')] `shouldBe` "a"

    it "repeats an element n times for weight n" $ do
      _weightedToUnweighted [(2, 'a')] `shouldBe` "aa"

    it "rolls out all elements" $ do
      _weightedToUnweighted [(1, 'a'), (2, 'b')] `shouldBe` "abb"
