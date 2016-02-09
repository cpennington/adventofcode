{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Advent where

import GHC.TypeLits
import Data.Proxy
import Data.Reflection (reifyNat, reifySymbol, reflect)
import qualified Data.Text as T
import System.Exit
import Control.Monad

strip = T.unpack . T.strip . T.pack

solvePuzzleWithTest day test expected solve = do
    let testResult = solve test
    when (testResult /= expected) $ die $ "Test failed: got " ++ show testResult

solvePuzzle day solve = do
    input <- readFile $ "input/" ++ show day ++ ".txt"
    print $ solve input

