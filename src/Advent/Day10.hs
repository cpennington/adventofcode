{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Advent.Day10 where

import Advent
import Data.List (group, iterate)

lookAndSay :: String -> String
lookAndSay input = concatMap lookAndSay1 $ group input
    where
        lookAndSay1 :: String -> String
        lookAndSay1 xs = show (length xs) ++ [head xs]

instance Solution 10 "a" where
    solve (Input input) = Output $ show $ length $ (iterate lookAndSay $ strip input) !! 40

instance Solution 10 "b" where
    solve (Input input) = Output $ show $ length $ (iterate lookAndSay $ strip input) !! 50
