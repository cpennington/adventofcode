module Advent.Day6 where

import Text.Parsec.Char (string, digit, endOfLine)
import Text.Parsec ((<|>), many, Parsec, parse, try)
import Data.List (foldl')
import Data.Set (empty, size, union, difference, intersection, fromList, Set)
import qualified Data.Set as S
import Data.Map (elems, fromList, empty, alter, Map)
import qualified Data.Map as M

data Point = Point !Int !Int
  deriving (Eq, Ord)
data Range = Range !Point !Point
  deriving (Eq, Ord)
data Action = TurnOn !Range | TurnOff !Range | Toggle !Range
  deriving (Eq, Ord)

commands = many command
command = do
  cmd <- turnOn <|> turnOff <|> toggle
  endOfLine
  return cmd

turnOn = _command "turn on" TurnOn
turnOff = _command "turn off" TurnOff
toggle = _command "toggle" Toggle

_command :: String -> (Range -> Action) -> Parsec String () Action
_command tag result = do
  try $ string tag
  string " "
  rng <- range
  return $ result rng

range :: Parsec String () Range
range = do
  ll <- point
  string " through "
  ur <- point
  return $ Range ll ur

point :: Parsec String () Point
point = do
  x <- many digit
  string ","
  y <- many digit
  return $ Point (read x) (read y)

explode :: Range -> [Point]
explode (Range (Point llx lly) (Point urx ury)) = [Point x y | x <- [llx..urx], y <- [lly..ury]]

updateA :: Set Point -> Action -> Set Point
updateA lights (TurnOn range) = union lights $ S.fromList $ explode range
updateA lights (TurnOff range) = difference lights $ S.fromList $ explode range
updateA lights (Toggle range) = difference (union lights goOn) goOff
  where
    toggled = S.fromList $ explode range
    goOn = difference toggled lights
    goOff = intersection lights toggled

on Nothing = Just 1
on (Just x) = Just $ x + 1
off Nothing = Nothing
off (Just 1) = Nothing
off (Just x) = Just $ x - 1
tgl Nothing = Just 2
tgl (Just x) = Just $ x + 2

updateB :: Map Point Int -> Action -> Map Point Int
updateB lights (TurnOn range) = foldl' (flip $ alter on) lights $ explode range
updateB lights (TurnOff range) = foldl' (flip $ alter off) lights $ explode range
updateB lights (Toggle range) = foldl' (flip $ alter tgl) lights $ explode range

solveA input = case parse commands "" input of
  Left err -> show err
  Right cmds -> show $ size $ foldl' updateA S.empty cmds

solveB input = case parse commands "" input of
  Left err -> show err
  Right cmds -> show $ sum $ elems $ foldl' updateB M.empty cmds
