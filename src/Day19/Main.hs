module Main where
import Prelude as P -- makes disambiguation a bit easier
import Data.Sequence as S
import Data.Maybe (fromJust)

input = 3001330

eval :: a -> a
eval a = a `seq` a

rotate s = (S.drop 1 s) >< (S.take 1 s)

step1 = rotate . S.deleteAt 1
step2 s =  rotate . S.deleteAt mid $ s
  where mid = S.length s `div` 2
step = step2

result seq= flip index 0 $ until ((== 1) . S.length) step seq

main = print . result $ fromList [1..input]