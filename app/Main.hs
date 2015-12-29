module Main where

import Lib
import System.Environment (getArgs)
import Advent.Day1
import Advent.Day2
import Advent.Day3

main :: IO ()
main = do
  [day, puzzle] <- getArgs

  input <- readFile $ "input/" ++ day ++ ".txt"

  let result = case (read day, puzzle) of
        (1, "a") -> show $ Advent.Day1.solveA input
        (1, "b") -> show $ Advent.Day1.solveB input
        (2, "a") -> show $ Advent.Day2.solveA input
        (2, "b") -> show $ Advent.Day2.solveB input
        (3, "a") -> show $ Advent.Day3.solveA input
        (3, "b") -> show $ Advent.Day3.solveB input
        _        -> day ++ puzzle ++ " isn't ready yet!"

  putStrLn result
