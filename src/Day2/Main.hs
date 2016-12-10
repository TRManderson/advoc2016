module Main where

import Data.List (foldl', scanl')
import Data.Foldable (fold)

pos :: [[Int]]
pos = [ [1, 2, 3]
      , [4, 5, 6]
      , [7, 8, 9]
      ]

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

bound1 :: Int -> Int
bound1 = max 0 . min 2

bound :: (Int, Int) -> (Int, Int)
bound (x, y) = (bound1 x, bound1 y)

transform :: Direction -> (Int, Int) -> (Int, Int)
transform d (x, y) = let (x', y') = movement d in
  bound (x+x', y+y')

foldDirs :: (Int, Int) -> [Direction] -> (Int, Int)
foldDirs init dirs = foldl' (flip transform) init dirs

parse :: String -> [Direction]
parse = map fromChar

solve :: [[Direction]] -> [Int]
solve dirs = tail $ map (\(x, y) -> pos !! x !! y) $ scanl' foldDirs (1, 1) dirs

main = do
  problem <- map parse . lines <$> getContents
  putStrLn . fold . map show . solve $ problem
  return ()