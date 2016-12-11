module Main where

import Data.List (sort, group, sortOn, transpose)

fn1 = last
fn2 = head
fn = fn2

solve = map (head . fn . sortOn length . group . sort) . transpose

main = do
  input <- lines <$> getContents
  putStrLn $ solve input