module Main where
import Data.Word
import Control.Parallel.Strategies


parFilter :: Int -> (a -> Bool) -> [a] -> [a]
parFilter n p = map fst . filter snd . parMap . map (\a -> (a, p a))
  where
    parMap = withStrategy (parBuffer n strat)
    strat = evalTuple2 r0 rdeepseq

parseLine :: String -> (Word32, Word32)
parseLine s = (parse part1, parse part2)
  where parse = fromInteger . read
        part1 = takeWhile (/= '-') s
        part2 = tail . dropWhile (/= '-') $ s

allIPs :: [Word32]
allIPs = [minBound..maxBound]

filterIPs :: (Word32, Word32) -> Word32 -> Bool
filterIPs (lower, upper) val = val < lower || val > upper

conj :: [a -> Bool] -> a -> Bool
conj bools a = all ($ a) bools

solve :: [(Word32, Word32)] -> [Word32]
solve = ($ allIPs) . parFilter 8 . conj . map filterIPs

answer1 :: [Word32] -> Word32
answer1 = head
answer2 :: [Word32] -> Int
answer2 = length
answer = answer1

main = do
  inputLines <- lines <$> getContents
  let result = answer . solve . map parseLine $ inputLines
  print result