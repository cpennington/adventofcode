{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

instance Solution 8 "a" where
    solve (Input input) = Output $ show $ sum (map length words) - sum (map (length . unencode) words)
      where words = filter ((>1) . length) $ lines input

instance Solution 8 "b" where
    solve (Input input) = Output $ show $ sum (map (length . encode) words) - sum (map length words)
      where words = filter ((>1) . length) $ lines input
