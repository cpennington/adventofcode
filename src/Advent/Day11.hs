#! /usr/bin/env stack
-- stack runghc

module Advent.Day11 where

import Advent

import Data.Char (ord, chr)
import Data.Digits (digits, unDigits)

newtype Password = Password { unPassword :: [Int] }

toPassword :: String -> Password
toPassword = Password . reverse . map pwdOrd

fromPassword :: Password -> String
fromPassword = map pwdChr . reverse . unPassword

succPassword :: Password -> Password
succPassword (Password p) = Password $ succ' p
    where
        succ' [] = []
        succ' (25:rest) = 0:(succ' rest)
        succ' (x:rest) = (x+1):rest

pwdOrd :: Char -> Int
pwdOrd c = ord c - (ord 'a')

pwdChr :: Int -> Char
pwdChr = chr . (+ ord 'a')

hasAscending (Password p) = asc' p
    where
        asc' [] = False
        asc' [_] = False
        asc' [_, _] = False
        asc' (x:y:z:rest) = (x-2 == z && y-1 == z) || (asc' $ y:z:rest)

hasI = elem (pwdOrd 'i') . unPassword
hasO = elem (pwdOrd 'o') . unPassword
hasL = elem (pwdOrd 'l') . unPassword

countPairs (Password p) = count' p
    where
        count' [] = 0
        count' [_] = 0
        count' (x:y:rest) | x == y = 1 + count' rest
                          | otherwise = count' $ y:rest

validPassword p = hasAscending p && not (hasI p || hasO p || hasL p) && countPairs p >= 2

solveA input = fromPassword $ head $ filter validPassword $ tail $ iterate succPassword $ toPassword $ strip input

main = do
    solvePuzzle 11 solveA
    solvePuzzle 11 (solveA . solveA)
