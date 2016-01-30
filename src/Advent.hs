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

newtype Input (d :: Nat) (p :: Symbol) = Input String
  deriving Show
newtype Output (d :: Nat) (p :: Symbol) = Output String
  deriving Show

class Solution d p where
  solve :: Input d p -> Output d p

puzzleInput :: Integer -> String -> IO (Input d p)
puzzleInput day puzzle = reifyNat day $ \d -> reifySymbol puzzle $ \p -> do
  input <- readFile $ "input/" ++ (show $ reflect d) ++ ".txt"
  return $ Input input

solvePuzzle :: (Solution d p) => Integer -> String -> IO (Output d p)
solvePuzzle day puzzle = do
  input <- puzzleInput day puzzle
  return $ solve input

strip = T.unpack . T.strip . T.pack
