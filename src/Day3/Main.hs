{-# LANGUAGE Safe #-}
module Main where
import Control.Monad (join)

predicate :: (Ord a, Num a) => (a, a, a) -> Bool
predicate (a, b, c) = a + b > c && a + c > b && b + c > a

getInput :: IO [(Int, Int, Int)]
getInput = do
  c <- map (map read . words) . lines <$> getContents
  return [(a, b, c) | [a, b, c] <- c]

reorient :: [(a, a, a)] -> [(a, a, a)]
reorient [] = []
reorient ((a,b,c):(d,e,f):(g,h,i):xs) = (a,d,g):(b,e,h):(c,f,i): reorient xs

main' =  print . length . filter predicate
main1 = join $ main' <$> getInput
main2 = join $ main' . reorient <$> getInput

main = main1