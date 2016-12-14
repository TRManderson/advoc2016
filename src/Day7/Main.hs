{-# LANGUAGE Safe #-}
module Main where
import Data.List.Split (splitOn)
import Data.List (intersect)
import Control.Monad (join, (>>=))

splitOut :: String -> [String]
splitOut s = splitOn "[" s >>= splitOn "]"

paired :: [String] -> ([String], [String])
paired [] = ([], [])
paired [x] = ([x], [])
paired (a:b:xs) = (a:a', b:b')
  where (a', b') = paired xs

-- Pt 1
abbaAny :: String -> Bool
abbaAny (a:b:c:d:xs) = (a == d && b == c && a /= b) || abbaAny (b:c:d:xs)
abbaAny _ = False

abba = (\(a,b) -> any abbaAny a && all (not . abbaAny) b) . paired . splitOut

main1 = do
 res <- length . filter abba . lines <$> getContents
 print res

-- Pt 2
aba2bab [a, b, _] = [b,a,b]

findAba :: String -> [String]
findAba (a:b:c:xs) = if a==c && a /= b then
    [a, b, c]: findAba (b:c:xs)
  else
    findAba (b:c:xs)
findAba _ = []

soln :: String -> Bool
soln = not . null . uncurry intersect . (\(a,b) -> (map aba2bab $ a >>= findAba, b >>= findAba)) . paired . splitOut

main2 = join $ print . length . filter soln . lines  <$> getContents

main = main2