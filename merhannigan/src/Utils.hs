{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Utils where

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State
import Data.List
import Data.Maybe
import System.IO.Unsafe
import System.Random

randomInState :: (StdGen -> (a, StdGen)) -> State StdGen a
randomInState f = do
  gen <- get
  let (result, nextGen) = f gen
  put nextGen
  return result

choose :: [a] -> State StdGen a
choose list = do
  index <- randomInState $ randomR (0, length list - 1)
  return $ list !! index

weighted :: [(Int, a)] -> State StdGen a
weighted list = choose $ _weightedToUnweighted list

_weightedToUnweighted :: [(Int, a)] -> [a]
_weightedToUnweighted list = case list of
  (weight, element) : rest ->
    replicate weight element ++ _weightedToUnweighted rest
  [] -> []

range :: Random a => (a, a) -> State StdGen a
range bounds = randomInState $ randomR bounds

suchThat :: State StdGen a -> (a -> Bool) -> State StdGen a
suchThat action predicate = do
  candidate <- action
  if predicate candidate
    then return candidate
    else suchThat action predicate

data Token context a
  = S
  | T a
  | NT context

toTerminals :: [Token context a] -> Maybe [a]
toTerminals = mapM $ \ token -> case token of
  T a -> Just a
  NT _ -> Nothing
  S -> Nothing

type Grammar context a = [Rule context a]

data Rule context a
  = Rule Int ([Token context a] -> Maybe [Token context a])

rule :: Int -> ([Token context a] -> [Token context a]) -> Rule context a
rule weight rule = Rule weight (fromUnsafe rule)

guarded :: Int -> ([Token context a] -> (Bool, [Token context a])) -> Rule context a
guarded weight rule = Rule weight $ \ tokens -> case fromUnsafe rule tokens of
  Nothing -> Nothing
  Just (False, _) -> Nothing
  Just (True, result) -> Just result

fromUnsafe :: (a -> b) -> (a -> Maybe b)
fromUnsafe f = \ tokens -> unsafePerformIO $
  handle (\ (PatternMatchFail _) -> return Nothing) $ do
    Just <$> evaluate (f tokens)

runGrammar :: Grammar context a -> State StdGen [a]
runGrammar rules = inner [S]
  where
    inner current = case toTerminals current of
      Just terminals -> return terminals
      Nothing -> inner =<< tryRules rules current

tryRules :: [Rule context a] -> [Token context a] -> State StdGen [Token context a]
tryRules rules current = case catMaybes $ map (\ rule -> tryRule rule current) rules of
  [] -> error "tryRules: no rule matches found"
  results -> weighted results

tryRule :: Rule context a -> [Token context a] -> Maybe (Int, [Token context a])
tryRule rule@(Rule weight innerRule) tokens = case tryOnInits innerRule tokens of
  Just result -> Just (weight, result)
  Nothing -> case tokens of
    token : rest -> fmap (second (token :)) $ tryRule rule rest
    [] -> Nothing

tryOnInits :: ([a] -> Maybe [a]) -> [a] -> Maybe [a]
tryOnInits f list = inner $ zip (inits list) (tails list)
  where
    inner prefixes = case prefixes of
      [] -> Nothing
      (prefix, suffix) : rest -> case f prefix of
        Just result -> Just $ result ++ suffix
        Nothing -> inner rest
