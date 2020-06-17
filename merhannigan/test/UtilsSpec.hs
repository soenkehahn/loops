{-# LANGUAGE ScopedTypeVariables #-}

module UtilsSpec where

import Test.Hspec
import Control.Exception
import Data.HashSet (empty, insert)
import Test.QuickCheck hiding (choose)
import Control.Monad.Trans.State
import Utils
import System.Random
import Control.Monad

spec :: Spec
spec = do
  describe "choose" $ do
    it "always picks an element of the given list" $ do
      property $ \ gen (list :: [Int]) -> do
        not (null list) ==>
          evalState (choose list) (mkStdGen gen) `shouldSatisfy` (`elem` list)

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

  describe "grammar" $ do
    it "allows to encode a single simple rule" $ do
      let grammar = rule 1 (\ [S] -> [T 42]) : []
      testGrammar grammar `shouldReturn` [42 :: Integer]

    it "works for multiple rules" $ do
      let grammar =
            rule 1 (\ [S] -> [NT 42]) :
            rule 1 (\ [NT 42] -> [T 23]) :
            []
      testGrammar grammar `shouldReturn` [23 :: Integer]

    it "works for rules matching starting in the middle of the sequence" $ do
      let grammar =
            rule 1 (\ [S] -> [T 23, NT 42]) :
            rule 1 (\ [NT 42] -> [T 23]) :
            []
      testGrammar grammar `shouldReturn` [23, 23 :: Integer]

    it "works for rules that match only a subsequence" $ do
      let grammar =
            rule 1 (\ [S] -> [NT False, NT True, NT False]) :
            rule 1 (\ [NT True] -> [T 42]) :
            rule 1 (\ [NT False] -> [T 23]) :
            []
      testGrammar grammar `shouldReturn` [23, 42, 23 :: Integer]

    it "allows a different type in non-terminals than in terminals" $ do
      let grammar =
            rule 1 (\ [S] -> [NT ()]) :
            rule 10 (\ [NT ()] -> [T 42]) :
            []
      testGrammar grammar `shouldReturn` [42 :: Integer]

    describe "overlapping rules" $ do
      it "picks one rule at random" $ do
        let grammar =
              rule 1 (\ [S] -> [T 1]) :
              rule 1 (\ [S] -> [T 2]) :
              []
        grammar `produces` [1 :: Integer]
        grammar `produces` [2]

      it "allows to specify weights for overlapping rules" $ do
        let grammar =
              rule 1 (\ [S] -> [T 1]) :
              rule 10 (\ [S] -> [T 2]) :
              []
        grammar `produces` [1 :: Integer]
        words <- replicateM 100 (testGrammar grammar)
        length (filter (== [2]) words) `shouldSatisfy` (> 50)

testGrammar :: Grammar context a -> IO [a]
testGrammar grammar = do
  gen <- newStdGen
  return $ evalState (runGrammar grammar) gen

produces :: (Eq a, Show a) => Grammar context a -> [a] -> IO ()
produces = inner (100 :: Int)
  where
    inner 0 _ word = error $ unlines $
      "produces: didn't produce the given word within 100 tries" :
      ("word: " ++ show word) :
      []
    inner n grammar word = do
      result <- testGrammar grammar
      if result == word
        then return ()
        else inner (n - 1) grammar word
