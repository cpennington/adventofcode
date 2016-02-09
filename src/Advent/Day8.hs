#! /usr/bin/env stack
-- stack runghc

module Advent.Day8 where

import Advent
import Data.Char (chr)
import Numeric (readHex)

unencode ('"':s) = _unencode s
  where
    _unencode ('\\':'"':s) = '"':_unencode s
    _unencode ('\\':'\\':s) = '\\':_unencode s
    _unencode ('\\':'x':a:b:s) = (chr $ (fst . head) parses):_unencode s
      where parses = readHex (a:b:[])
    _unencode ('"':[]) = ""
    _unencode (c:[]) = error "must end in '\"'"
    _unencode (c:s) = c:_unencode s
    _unencode x = error $ "WTF '" ++ x ++ "'"
unencode s = error $ "must start with '\"', was '" ++ s ++ "'"

encode s = '"':_encode s
  where
    _encode ('\\':s) = '\\':'\\':_encode s
    _encode ('"':s) = '\\':'"':_encode s
    _encode [] = '"':[]
    _encode (c:s) = c:_encode s

solveA input = sum (map length words) - sum (map (length . unencode) words)
  where words = filter ((>1) . length) $ lines input

solveB input = sum (map (length . encode) words) - sum (map length words)
  where words = filter ((>1) . length) $ lines input

main = do
    solvePuzzle 8 solveA
    solvePuzzle 8 solveB
