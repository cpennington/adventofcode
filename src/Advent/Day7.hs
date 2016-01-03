module Advent.Day7 where

import Prelude hiding (lookup)
import Text.Parsec (parse, choice, try, (<|>), Parsec)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (string, digit, letter, space, endOfLine)
import Data.Map (insert, empty, lookup, fromList, Map)
import Data.Word (Word16)
import Data.Bits (complement, (.&.), (.|.), shift)
import Control.Monad.State.Lazy (get, put, evalState, State)

type Wire = String
type Value = Word16
type Wiring = Map Wire Instruction
type Results = Map Wire Value
data Atom = Wire Wire | Value Value
  deriving Show
data Instruction = Feed Atom
                 | And Atom Atom
                 | Or Atom Atom
                 | LShift Atom Int
                 | RShift Atom Int
                 | Not Atom
  deriving Show
type WiringParser = Parsec String () (Wire, Instruction)
type InstParser = Parsec String () Instruction


wire :: Parsec String () Wire
wire = many1 letter

value :: (Integral a, Read a) => Parsec String () a
value = many1 digit >>= (return . read)

atom :: Parsec String () Atom
atom = (Wire <$> wire) <|> (Value <$> value)

wiresP = many1 wiringP

wiringP :: WiringParser
wiringP = do
  i <- choice [notP, andP, orP, lshiftP, rshiftP, feedP]
  string " -> "
  w <- wire
  endOfLine
  return $ (w, i)

feedP :: InstParser
feedP = do
  v <- try atom
  return $ Feed v

andP :: InstParser
andP = do
  left <- try $ do
    left <- atom
    string " AND "
    return left
  right <- atom
  return $ And left right

orP :: InstParser
orP = do
  left <- try $ do
    left <- atom
    string " OR "
    return left
  right <- atom
  return $ Or left right

lshiftP :: InstParser
lshiftP = do
  left <- try $ do
    left <- atom
    string " LSHIFT "
    return left
  val <- value
  return $ LShift left val

rshiftP :: InstParser
rshiftP = do
  left <- try $ do
    left <- atom
    string " RSHIFT "
    return left
  val <- value
  return $ RShift left val

notP :: InstParser
notP = do
  try $ string "NOT "
  w <- atom
  return $ Not w

evalWire :: Wire -> [(Wire, Instruction)] -> Value
evalWire w ws = evalState (lookupAtom (Wire w)) (fromList ws, empty)

lookupAtom :: Atom -> State (Wiring, Results) Value
lookupAtom (Value v) = return v
lookupAtom (Wire w) = do
  (wiringMap, results) <- get
  case lookup w results of
    Just val -> return val
    Nothing -> do
      case lookup w wiringMap of
        Nothing -> error $ "insufficient specification, missing " ++ w
        Just wiring -> do
            val <- evalInst wiring
            (wiringMap', results') <- get
            put (wiringMap', insert w val results')
            return val

evalInst :: Instruction -> State (Wiring, Results) Value
evalInst (Feed a) = lookupAtom a
evalInst (Not atom) = complement <$> lookupAtom atom
evalInst (And left right) = (.&.) <$> lookupAtom left <*> lookupAtom right
evalInst (Or left right) = (.|.) <$> lookupAtom left <*> lookupAtom right
evalInst (LShift wire val) = flip shift val <$> lookupAtom wire
evalInst (RShift wire val) = flip shift (-val) <$> lookupAtom wire

solveA input = case parse wiresP "" input of
  Left err -> show err
  Right ws -> show $ evalWire "a" ws

solveB input = case parse wiresP "" input of
  Left err -> show err
  Right ws -> show $ evalWire "a" (ws ++ [("b", Feed $ Value $ evalWire "a" ws)])
