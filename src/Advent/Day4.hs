#! /usr/bin/env stack
-- stack runghc

module Advent.Day4 where

import Advent

import Prelude hiding (init)
import Crypto.Hash (digestToHexByteString, hashUpdate, hashFinalize, hashInitAlg, MD5(..))
import Data.List (findIndex)
import qualified Data.ByteString.Char8 as B

check :: Int -> String -> Int -> Bool
check count base num = B.isPrefixOf target $ digestToHexByteString $ hashFinalize $ hashUpdate baseHash $ B.pack $ show num
  where baseHash = hashUpdate (hashInitAlg MD5) (B.pack base)
        target = B.pack $ replicate count '0'

solveA input = findIndex (check 5 $ strip input) [0..]
solveB input = findIndex (check 6 $ strip input) [0..]

main = do
    solvePuzzle 4 solveA
    solvePuzzle 4 solveB
