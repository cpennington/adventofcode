#! /usr/bin/env stack
-- stack runghc

{-# LANGUAGE OverloadedStrings #-}

module Advent.Day12 where

import Advent

import Data.Aeson (Value(..), decode)
import Data.HashMap.Strict (elems)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (pack)

import Debug.Trace

sumJSON (Object o) = sum $ map sumJSON $ elems o
sumJSON (Array a) = sum $ fmap sumJSON a
sumJSON (Number n) = n
sumJSON _ = 0

removeRed (Object o) | elem (String "red") $ elems o = Null
                     | otherwise = Object $ fmap removeRed o
removeRed (Array a) = Array $ fmap removeRed a
removeRed v = v

solveA input = sumJSON $ fromJust $ decode $ pack input
solveB input = sumJSON $ removeRed $ fromJust $ decode $ pack input

main = do
    solvePuzzle 12 solveA
    solvePuzzle 12 solveB
