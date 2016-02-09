#! /usr/bin/env stack
-- stack runghc

module Advent.Day1 where

import Advent

value :: Char -> Int
value '(' = 1
value ')'=  -1
value _ = 0

solveA :: String -> Int
solveA input = sum $ map value input

solveB :: String -> Int
solveB input = length $ takeWhile (>= 0) $ scanl (+) 0 $ map value input

main = do
    solvePuzzle 1 solveA
    solvePuzzle 1 solveB
