{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (mapMaybe)

rowParser :: Parser (Int, Int)
rowParser = do
  string "Disc #"
  decimal
  string " has "
  pos <- decimal
  string " positions; "
  string "at time=0, it is at position "
  initial <- decimal
  char '.'
  return (initial, pos)


fromList :: [(Int, Int)] -> (Int, [Int], [Int])
fromList ls = (0, vals, steps)
  where (vals, steps) = unzip ls

step :: (Int, [Int], [Int]) -> (Int, [Int], [Int])
step (n, vals, steps) = (n+1, steppedVals, steps)
  where steppedVals = zipWith (\a b -> (a + 1) `mod` b) vals steps

isDone (_, vals, steps) = vals == endVals
  where endVals = zipWith (flip (-)) [1..] steps

main = do
  contents <- T.lines <$> T.getContents
  let inputData = mapMaybe (maybeResult . flip feed "" . parse rowParser) contents
  let (soln, _, _) = until isDone step . fromList $ inputData
  print soln