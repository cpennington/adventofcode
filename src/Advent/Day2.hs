#! /usr/bin/env stack
-- stack runghc

module Advent.Day2 where

import Advent
import Data.List.Split (splitOn)

type Box = (Int, Int, Int)

parseAll :: String -> [Box]
parseAll = map parse . words

parse :: String -> Box
parse input = (x, y, z)
  where [x, y, z] = map read $ splitOn "x" input

areas :: Box -> [Int]
areas (x, y, z) = [x*y, y*z, x*z]

perimeters :: Box -> [Int]
perimeters (x, y, z) = map (*2) [x + y, x + z, y + z]

volume :: Box -> Int
volume (x, y, z) = x * y * z

solveA :: String -> Int
solveA = sum . map amount . parseAll
  where amount box = 2 * sum (areas box) + minimum (areas box)

solveB :: String -> Int
solveB = sum . map amount . parseAll
  where amount box = minimum (perimeters box) + volume box

main = do
    solvePuzzle 2 solveA
    solvePuzzle 2 solveB
