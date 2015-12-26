{-# LANGUAGE OverloadedStrings #-}
module Day7 (day7, day7', run) where

import Control.Applicative ((<|>), optional)
import Control.Monad (liftM2)
import Data.Attoparsec.Text
    ( Parser
    , char
    , choice
    , decimal
    , endOfLine
    , isEndOfLine
    , isHorizontalSpace
    , many'
    , parseOnly
    , string
    , takeTill
    )
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Function (fix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, insert, keysSet, lookup)
import qualified Data.Map.Lazy as LazyMap ((!), fromSet)
import Data.Set (Set)
import Data.Text (Text, pack, unpack)

-- The function here is two parts:
-- 1. parseInstructions - Parse the text into a dictionary of Instructions,
-- whose keys are References to wires and whose values are the associated
-- Expressions
-- 2. resolve - resolves a set of Instructions and a Reference to a wire to a
-- final signal value on that wire
-- The resolve function uses resolve' (which performs map lookups), and evaluate
-- (which performs and/or/shift logic) co-recursively, resolving and memoizing
-- wire references as it goes
day7 :: String -> Int
day7 input = case parseAndApply input of
    (Right i) -> i
    _         -> -1

parseAndApply :: String -> Either String Int
parseAndApply input = do
    instructions <- parseOnly parseInstructions (pack input)
    resolve instructions "a"

-- Begin section: parsing instructions into a Map (dictionary)
data Expression =
    JunctionEx JunctionOp Expression Expression |
    ShiftEx ShiftOp Expression Int |
    NotEx Expression |
    ReferenceEx Reference |
    SignalEx Int
    deriving Show
data JunctionOp = AndOp | OrOp deriving Show
data ShiftOp = LShiftOp | RShiftOp deriving Show
type Reference = Text
type Instruction = (Reference, Expression)
type Instructions = Map Reference Expression

parseInstructions :: Parser Instructions
parseInstructions = fmap Map.fromList . many' $ parseInstruction <* optional endOfLine

parseInstruction :: Parser Instruction
parseInstruction = do
    e <- parseExpression
    string " -> "
    wireLabel <- takeTill isEndOfLine
    return (wireLabel, e)

parseExpression :: Parser Expression
parseExpression = choice [parseJunctionEx, parseShiftEx, parseNotEx, parseSignalOrReference]

parseSignalOrReference :: Parser Expression
parseSignalOrReference = choice [parseSignalEx, parseReferenceEx]

parseJunctionEx :: Parser Expression
parseJunctionEx = do
    a <- parseSignalOrReference
    char ' '
    op <- (string "AND" >> return AndOp) <|> (string "OR"  >> return OrOp)
    char ' '
    b <- parseSignalOrReference
    return $ JunctionEx op a b

parseShiftEx :: Parser Expression
parseShiftEx = do
    a <- parseSignalOrReference
    char ' '
    op <- (string "LSHIFT" >> return LShiftOp) <|> (string "RSHIFT" >> return RShiftOp)
    char ' '
    b <- decimal
    return $ ShiftEx op a b

parseNotEx :: Parser Expression
parseNotEx = do
    string "NOT "
    a <- parseSignalOrReference
    return $ NotEx a

parseReferenceEx :: Parser Expression
parseReferenceEx = do
    r <- parseReference
    return $ ReferenceEx r

parseSignalEx :: Parser Expression
parseSignalEx = do
    s <- decimal
    return $ SignalEx s

parseReference :: Parser Reference
parseReference = takeTill isHorizontalSpace
-- End section on parsing

-- The meat of the actual algorithm starts here
-- evaluate is responsible for evaluating Expressions. Given a function for
-- resolving References and an Expression (which might contain References), it
-- performs the logic to request Reference resolution and combine the results.
evaluate :: (Reference -> Either String Int) -> Expression -> Either String Int
evaluate res (SignalEx s) = Right s
evaluate res (ReferenceEx r) = res r
evaluate res (NotEx ex) = fmap complement (evaluate res ex)
evaluate res (ShiftEx op ex x) = fmap shiftFunc (evaluate res ex)
    where
        shiftFunc = case op of
            LShiftOp -> flip shiftL x
            RShiftOp -> flip shiftR x
evaluate res (JunctionEx op ex1 ex2) = liftM2 junctionFunc (evaluate res ex1) (evaluate res ex2)
    where
        junctionFunc = case op of
            AndOp -> (.&.)
            OrOp  -> (.|.)

-- resolve takes a list of instructions and a reference to a wire (e.g. "aq")
-- and returns the signal on that wire. Intermediate calculations are memoized,
-- so that the calculation actually finishes.
resolve :: Instructions -> Reference -> Either String Int
resolve store = fix $ memoize (Map.keysSet store) . resolve' store

memoize :: Set Reference -> (Reference -> a) -> Reference -> a
memoize keys f key = LazyMap.fromSet f keys LazyMap.! key

resolve' :: Instructions -> (Reference -> Either String Int) -> Reference -> Either String Int
resolve' store f ref = case Map.lookup ref store of
    (Just ex) -> evaluate f ex
    Nothing   -> Left $ "Failed to resolve " ++ unpack ref

day7' :: String -> Int
day7' input = case parseAndApply' input of
    (Right i) -> i
    _         -> -1

parseAndApply' input = do
    initialInstructions <- parseOnly parseInstructions (pack input)
    aSignal <- resolve initialInstructions "a"
    let modifiedInstructions = Map.insert "b" (SignalEx aSignal) initialInstructions
    resolve modifiedInstructions "a"

-- Input
run :: IO ()
run = do
    putStrLn "Day 7 results: "
    input <- readFile "inputs/day7.txt"
    putStrLn $ "  " ++ show (day7 input)
    putStrLn $ "  " ++ show (day7' input)
