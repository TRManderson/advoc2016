{-# LANGUAGE Safe #-}
module Main where
import Data.Ix (inRange)
import Data.List (foldl', scanl')
import Data.Foldable (fold)
import Data.Char (chr)
import Data.Array
import Control.Monad (join)
import Data.Maybe (mapMaybe)

(!!!) :: Ix i => Array i e -> i -> Maybe e
a !!! idx = if bounds a `inRange` idx then
    Just $ a ! idx
  else
    Nothing

type Problem = Array (Int, Int) (Maybe Char)

partA :: Problem
partA = listArray ((0, 0), (2, 2)) $ map (Just . head . show) [1..9]
partAinit = (1, 1)

partB :: Problem
partB = listArray ((0, 0), (4, 4))
  [Nothing, Nothing, Just '1', Nothing, Nothing,
  Nothing, Just '2', Just '3', Just '4', Nothing,
  Just '5', Just '6', Just '7', Just '8', Just '9',
  Nothing, Just 'A', Just 'B', Just 'C', Nothing,
  Nothing, Nothing, Just 'D', Nothing, Nothing]
partBinit = (2, 2)

data Direction = U | D | L | R deriving (Show, Eq)

fromChar :: Char -> Direction
fromChar 'U' = U
fromChar 'R' = R
fromChar 'L' = L
fromChar 'D' = D
fromChar _ = undefined

movement :: Direction -> (Int, Int)
movement U = (-1, 0)
movement D = (1, 0)
movement L = (0, -1)
movement R = (0, 1)

transform :: Problem -> Direction -> (Int, Int) -> (Int, Int)
transform p d (x, y) = let (x', y') = movement d in
  case join (p !!! (x+x', y+y')) of
    Just _ -> (x+x', y+y')
    Nothing -> (x, y)


foldDirs :: Problem -> (Int, Int) -> [Direction] -> (Int, Int)
foldDirs p = foldl' (flip $ transform p)

parse :: String -> [Direction]
parse = map fromChar

solve :: Problem -> (Int, Int) -> [[Direction]] -> String
solve p init = tail . mapMaybe (join . (!!!) p) . scanl' (foldDirs p) init

main = do
  problem <- map parse . lines <$> getContents
  putStrLn . solve partB partBinit $ problem
  return ()