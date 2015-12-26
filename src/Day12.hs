{-# LANGUAGE OverloadedStrings #-}
module Day12 (day12, day12', run) where

import Data.Aeson
    ( Value
        ( Array
        , Number
        , Object
        , String
        )
    , decode'
    )
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Scientific
import Data.Text (Text)

sumJson :: Value -> Scientific
sumJson (Object o) = foldl addNumbers 0 o
sumJson (Array a)  = foldl addNumbers 0 a
sumJson (Number n) = n
sumJson _          = 0

addNumbers :: Scientific -> Value -> Scientific
addNumbers x o@(Object _) = x + sumJson o
addNumbers x a@(Array _)  = x + sumJson a
addNumbers x (Number n)   = x + n
addNumbers x _            = x

sumJson' :: Value -> Scientific
sumJson' (Object o) = fromMaybe 0 $ foldl addNumbers' (Just 0) o
sumJson' (Array a)  = foldl addNumbers'' 0 a
sumJson' (Number n) = n
sumJson' _          = 0

addNumbers' :: Maybe Scientific ->  Value -> Maybe Scientific
addNumbers' Nothing _          = Nothing
addNumbers' x       (String s) = if s == "red" then Nothing else x
addNumbers' x       v          = fmap (`addNumbers''` v) x

addNumbers'' :: Scientific -> Value -> Scientific
addNumbers'' x o@(Object _) = x + sumJson' o
addNumbers'' x a@(Array _)  = x + sumJson' a
addNumbers'' x (Number n)   = x + n
addNumbers'' x _            = x

day12 :: String -> Scientific
day12 input = maybe (-1) sumJson (decode' . pack $ input)

day12' :: String -> Scientific
day12' input = maybe (-1) sumJson' (decode' . pack $ input)

-- Input
run :: IO ()
run = do
    putStrLn "Day 12 results: "
    input <- readFile "inputs/day12.txt"
    putStrLn $ "  " ++ show (day12 input)
    putStrLn $ "  " ++ show (day12' input)
