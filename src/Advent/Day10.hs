#! /usr/bin/env stack
-- stack runghc

module Advent.Day10 where

import Advent
import Data.List (group, iterate)

lookAndSay :: String -> String
lookAndSay input = concatMap lookAndSay1 $ group input
    where
        lookAndSay1 :: String -> String
        lookAndSay1 xs = show (length xs) ++ [head xs]

lookAndSayN n input = length $ (iterate lookAndSay $ strip input) !! n

solveA input = length $ (iterate lookAndSay $ strip input) !! 40
solveB input = length $ (iterate lookAndSay $ strip input) !! 50

main :: IO ()
main = do
    solvePuzzle 10 $ lookAndSayN 40
    solvePuzzle 10 $ lookAndSayN 50
