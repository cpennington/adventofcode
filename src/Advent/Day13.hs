#! /usr/bin/env stack
-- stack runghc

module Advent.Day13 where

import Advent

import Text.Parsec (many, parse, (<|>), Parsec)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (string, letter, space, digit, endOfLine)
import Data.List (permutations, nub)
import Data.Map (fromList, keys, (!), union)

import Debug.Trace

seatings = do
    seats <- many seating
    return $ fromList seats

seating ::  Parsec String () ((String, String), Integer)
seating = do
    name <- many1 letter
    string " would "
    dir <- string "gain" <|> string "lose"
    space
    value <- many1 digit
    string " happiness units by sitting next to "
    nextTo <- many1 letter
    string "."
    endOfLine
    return $ case dir of
        "gain" -> ((name, nextTo), read value)
        "lose" -> ((name, nextTo), -(read value))

guests seats = nub $ map fst $ keys seats

neighbors arrangement = zip arrangement (tail arrangement ++ [head arrangement])
evalNeighbors seats (left, right) = seats ! (left, right) + seats ! (right, left)

evalArrangement seats arrangement = sum $ map (evalNeighbors seats) (neighbors arrangement)

testInput = unlines [
    "Alice would gain 54 happiness units by sitting next to Bob.",
    "Alice would lose 79 happiness units by sitting next to Carol.",
    "Alice would lose 2 happiness units by sitting next to David.",
    "Bob would gain 83 happiness units by sitting next to Alice.",
    "Bob would lose 7 happiness units by sitting next to Carol.",
    "Bob would lose 63 happiness units by sitting next to David.",
    "Carol would lose 62 happiness units by sitting next to Alice.",
    "Carol would gain 60 happiness units by sitting next to Bob.",
    "Carol would gain 55 happiness units by sitting next to David.",
    "David would gain 46 happiness units by sitting next to Alice.",
    "David would lose 7 happiness units by sitting next to Bob.",
    "David would gain 41 happiness units by sitting next to Carol."
    ]

maximizeSeats seats = maximum $ map (evalArrangement seats) $ permutations $ guests seats

solveA input = case parse seatings "" input of 
    Left err -> show err
    Right seats -> show $ maximizeSeats seats

solveB input = case parse seatings "" input of 
    Left err -> show err
    Right seats -> show withMe
        where
            withMe = maximizeSeats seatsWithMe
            seatsWithMe = union seats $ fromList $ zip (zip (repeat "Me") others ++ zip others (repeat "Me")) (repeat 0)
            others = guests seats

main = do
    print $ solveA testInput
    solvePuzzle 13 solveA
    solvePuzzle 13 solveB
