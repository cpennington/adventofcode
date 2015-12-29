module Advent.Day1 where

value :: Char -> Int
value '(' = 1
value ')'=  -1
value _ = 0

solveA :: String -> Int
solveA input = sum $ map value input

solveB :: String -> Int
solveB input = length $ takeWhile (>= 0) $ scanl (+) 0 $ map value input

