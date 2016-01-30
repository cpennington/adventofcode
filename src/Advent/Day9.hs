{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Advent.Day9 where

import Advent

import Data.Map (Map, fromList, adjust, insert, empty, assocs, filterWithKey, singleton, insertWith, (!))
import qualified Data.Map as M
import Data.List (foldl')
import Data.Maybe (isJust, fromJust)
import Data.Function (on)

import Debug.Trace

traceResult a b = traceShow (a, b) b

type TSMap = Map String (Map String Integer)
type PathAccum a c = Path a c -> Maybe (Path a c) -> ChoiceTree (Step a c) -> Maybe (Path a c)
type PathCmp a c = Path a c -> Path a c -> Bool

data Step a c = Step
    { sStep :: a
    , sCost :: c
    } deriving Show
data Path a c = Path
    { pSteps :: [a]
    , pCost :: c
    } deriving Show

data ChoiceTree a = Choice
    { cNode :: a
    , cNext :: [ChoiceTree a]
    } deriving Show

parseAll :: String -> TSMap
parseAll input = foldl' (flip addEntry) empty parsed
  where
    parsed = map parse $ lines input
    addEntry (from, to, dist) = add from to dist . add to from dist
    add a b d = insertWith (\k -> insert b d) a (singleton b d)

parse :: String -> (String, String, Integer)
parse = _parse . words
  where
    _parse [from, "to", to, "=", dist] = (from, to, read dist)
    _parse x = error $ "invalid string " ++ show x

paths :: TSMap -> [ChoiceTree (Step String Integer)]
paths m = paths' Nothing m
  where
    paths' Nothing m' = [ Choice (Step loc 0) subChoices
                       | (loc, steps) <- assocs m'
                       , let subChoices = paths' (Just loc) $ stripLoc loc m'
                       ]
    paths' (Just prev) m' = [ Choice (Step loc (m ! prev ! loc)) subChoices
                         | (loc, steps) <- assocs m'
                         , let subChoices = paths' (Just loc) $ stripLoc loc m'
                         ]
    stripLoc loc m = M.map (removeKey loc) $ removeKey loc m
    removeKey key = filterWithKey $ \k v -> k /= key

(Path steps c) .+. (Step step c') = Path (step:steps) (c + c')

betterPath :: PathCmp a c -> Maybe (Path a c) -> Maybe (Path a c) -> Maybe (Path a c)
betterPath isBetter Nothing r = r
betterPath isBetter l Nothing = l
betterPath isBetter (Just l) (Just r)
    | l `isBetter` r = Just l
    | otherwise = Just r

bestPath :: (Num c, Show a, Show c) => PathAccum a c -> Path a c -> Maybe (Path a c) -> [ChoiceTree (Step a c)] -> Maybe (Path a c)
bestPath isBetter current best steps = foldl' (isBetter current) best steps

tsp :: PathAccum String Integer -> TSMap -> Integer
tsp isBetter m = case bestPath isBetter (Path [] 0) Nothing (paths m) of
    Nothing -> error "Unable to find a path"
    Just (Path p c) -> c

isBetter :: (Show a, Show c, Num c) => PathCmp a c -> PathCmp a c -> PathAccum a c
isBetter _ _ current Nothing (Choice node []) = traceResult (current, Nothing :: Maybe (Step String Integer), node) $ Just $ current .+. node
isBetter better strictlyBetter current Nothing choice = traceResult (current, Nothing :: Maybe (Step String Integer), choice) $ bestPath (isBetter better strictlyBetter) (current .+. cNode choice) Nothing (cNext choice)
isBetter better strictlyBetter current (Just best) choice = traceResult (current, best, choice) $ if best `strictlyBetter` current'
    then Just best
    else case cNext choice of
            [] -> betterPath better (Just best) (Just current')
            next -> bestPath (isBetter better strictlyBetter) current' (Just best) next
    where
        current' = current .+. cNode choice

instance Solution 9 "a" where
    solve (Input input) = Output $ show $ tsp (isBetter (.<.) (.<.)) $ parseAll input
        where (.<.) = (<) `on` pCost

instance Solution 9 "b" where
    solve (Input input) = Output $ show $ tsp (isBetter ((>) `on` pCost) (const $ const False)) $ parseAll input
