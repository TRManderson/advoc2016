{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Main where
import Data.List
import Data.Char
import Control.Monad (join)

brackets :: String -> (Int, Int)
brackets xs = (num1, num2)
  where num1 = read $ takeWhile isDigit xs
        num2 = read . tail . dropWhile (/= 'x') $ xs

convert :: String -> Int
convert ('(':xs) = reps * inner repeatedPart + convert remaining
  where
    (len, reps) = brackets . takeWhile (/= ')') $ xs
    afterBrackets = tail . dropWhile (/= ')') $ xs
    repeatedPart = take len afterBrackets
    remaining = drop len afterBrackets
    innerPt1 = length :: String -> Int
    innerPt2 = convert
    inner = innerPt2
convert (x:xs) = 1 + convert xs
convert [] =0

main = join $ print . convert <$> getLine