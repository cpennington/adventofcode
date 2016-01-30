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

type TSMap = Map String (Map String Integer)
type PathCmp a c = Path a c -> Path a c -> Bool

data Step a c = Step a c
    deriving Show
data Path a c = Path [a] c
    deriving Show

data ChoiceTree a = Choice a [ChoiceTree a]
  deriving Show

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

bestPath :: (Num c, Show a, Show c) => PathCmp a c -> Maybe (Path a c) -> Path a c -> [ChoiceTree (Step a c)] -> Maybe (Path a c)
bestPath isBetter best current steps = best''
    where best' = bestPathHead isBetter best current steps
          best'' = bestPathTail isBetter best' current steps

bestPathHead :: (Num c, Show a, Show c) => PathCmp a c -> Maybe (Path a c) -> Path a c -> [ChoiceTree (Step a c)] -> Maybe (Path a c)
bestPathHead isBetter best current [] = traceShow ("head, []", best, current, result) result
    where result = betterPath isBetter best (Just current)
bestPathHead isBetter best current s@((Choice step cs):_) = traceShow ("head, cs", best, current, s, result) result
    where result = if (isJust best) && (fromJust best) `isBetter` next
                     then best
                     else bestPath isBetter best next cs
          next = current .+. step

bestPathTail :: (Num c, Show a, Show c) => PathCmp a c -> Maybe (Path a c) -> Path a c -> [ChoiceTree (Step a c)] -> Maybe (Path a c)
bestPathTail isBetter best current [] = traceShow ("tail, []", best, current, result) result
    where result = best
bestPathTail isBetter best current s@[_] = traceShow ("tail, [_]", best, current, s, result) result
    where result = best
bestPathTail isBetter best current s@(_:cs) = traceShow ("tail, cs", best, current, s, result) result
    where result = bestPath isBetter best current cs

-- shortest :: (Num c, Ord c) => Maybe (Path a c) -> c -> [ChoiceTree (Step a c)] -> Maybe (Path a c)
-- shortest minPath curCost [] = minPath
-- shortest minPath curCost ((Choice (Step step stepCost) []):[]) = betterPath minPath $ Just $ Path [step] (curCost + stepCost)
-- shortest minPath curCost ((Choice (Step step stepCost) next):[]) = shortest minPath (curCost + stepCost) next
-- shortest minPath curCost ((Choice (Step step stepCost) []):rest) = shortest (betterPath minPath takeStep) curCost rest
--     where takeStep = Just $ Path [step] (curCost + stepCost)
-- shortest Nothing curCost ((Choice (Step step stepCost) next):rest) = case shortest Nothing (curCost + stepCost) next of
--         Nothing -> Nothing
--         Just (Path path cost) ->
--             let takeStep = Just $ Path (step:path) cost
--             in case shortest takeStep curCost rest of
--             Nothing -> Just $ Path (step:path) cost
--             Just p -> Just p

-- shortest minPath@(Just (Path _ minCost)) curCost ((Choice (Step step stepCost) next):rest) = if curCost + stepCost > minCost
--     then fallbackPath
--     else case shortest minPath (curCost + stepCost) next of
--         Nothing -> fallbackPath
--         Just (Path path cost) ->
--             let takeStep = Just $ Path (step:path) cost
--             in case shortest takeStep curCost rest of
--             Nothing -> takeStep
--             Just p -> Just p
--     where fallbackPath = shortest minPath curCost rest

tsp :: PathCmp String Integer -> TSMap -> Integer
tsp isBetter m = case bestPath isBetter Nothing (Path [] 0) (traceShowId $ paths m) of
    Nothing -> error "Unable to find a path"
    Just (Path p c) -> c

instance Solution 9 "a" where
    solve (Input input) = Output $ show $ tsp ((<) `on` cost) $ parseAll input
        where cost (Path a c) = c

instance Solution 9 "b" where
    solve (Input input) = Output $ show $ tsp ((>) `on` cost) $ parseAll input
        where cost (Path a c) = c
