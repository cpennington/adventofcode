#! /usr/bin/env stack
-- stack runghc

module Advent.Day5 where

import Advent
import Data.List (isInfixOf)

banned :: String -> Bool
banned input = not $ or $ filters
  where filters = zipWith isInfixOf ["ab", "cd", "pq", "xy"] (repeat input)

vowels :: String -> Bool
vowels = (>= 3) . length . filter isVowel

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

twice :: String -> Bool
twice (x:y:rest) = x == y || twice (y:rest)
twice _ = False

surround :: String -> Bool
surround (x:y:z:rest) = x == z || surround (y:z:rest)
surround _ = False

repeatPairs :: String -> Bool
repeatPairs (x:y:rest) = (isInfixOf (x:y:[]) rest) || repeatPairs (y:rest)
repeatPairs _ = False

solveA :: String -> Int
solveA = length . filter vowels . filter twice . filter banned . words

solveB :: String -> Int
solveB = length . filter surround . filter repeatPairs . words

main = do
    solvePuzzle 5 solveA
    solvePuzzle 5 solveB
