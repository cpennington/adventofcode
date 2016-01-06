{-# LANGUAGE DataKinds #-}
module Main where

import Lib
import System.Environment (getArgs)
import Advent
import Advent.Day1
import Advent.Day2
import Advent.Day3
import Advent.Day4
import Advent.Day5
import Advent.Day6
import Advent.Day7
import Advent.Day8
import Data.Reflection (reflect, reifyNat)

import Data.Reflection (reifyNat, reifySymbol, reflect)

main :: IO ()
main = do
  [day, puzzle] <- getArgs

  input <- readFile $ "input/" ++ day ++ ".txt"

  result <- case (read day, puzzle) of
        (1, "a") -> return $ show $ Advent.Day1.solveA input
        (1, "b") -> return $ show $ Advent.Day1.solveB input
        (2, "a") -> return $ show $ Advent.Day2.solveA input
        (2, "b") -> return $ show $ Advent.Day2.solveB input
        (3, "a") -> return $ show $ Advent.Day3.solveA input
        (3, "b") -> return $ show $ Advent.Day3.solveB input
        (4, "a") -> return $ show $ Advent.Day4.solveA input
        (4, "b") -> return $ show $ Advent.Day4.solveB input
        (5, "a") -> return $ show $ Advent.Day5.solveA input
        (5, "b") -> return $ show $ Advent.Day5.solveB input
        (6, "a") -> return $ Advent.Day6.solveA input
        (6, "b") -> return $ Advent.Day6.solveB input
        (7, "a") -> show <$> (solvePuzzle 7 "a" :: IO (Output 7 "a"))
        (7, "b") -> show <$> (solvePuzzle 7 "b" :: IO (Output 7 "a"))
        (8, "a") -> show <$> (solvePuzzle 8 "a" :: IO (Output 8 "a"))
        (8, "b") -> show <$> (solvePuzzle 8 "b" :: IO (Output 8 "b"))
        _        -> return $ day ++ puzzle ++ " isn't ready yet!"

  putStrLn result
