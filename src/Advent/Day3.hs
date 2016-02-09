#! /usr/bin/env stack
-- stack runghc

module Advent.Day3 where

import Advent
import Data.Set (fromList)

data Dir = N | S | E | W
type Loc = (Int, Int)

parseAll :: String -> [Dir]
parseAll = map parse

parse :: Char -> Dir
parse '^' = N
parse '<' = W
parse '>' = E
parse 'v' = S

move :: Loc -> Dir -> Loc
move (x, y) N = (x, y+1)
move (x, y) S = (x, y-1)
move (x, y) E = (x+1, y)
move (x, y) W = (x-1, y)


split :: [a] -> ([a], [a])
split = foldr split' ([], [])
  where split' x (es, os) = (os, x:es)

santa = scanl move (0, 0)
robo dirs = santa es ++ santa os
  where (es, os) = split dirs
solveA = length . fromList . santa . parseAll
solveB = length . fromList . robo . parseAll

main = do
    solvePuzzle 3 solveA
    solvePuzzle 3 solveB
