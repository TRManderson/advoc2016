module Main where

input = "10011111011011001"
genList ('0':xs) = False : genList xs
genList ('1':xs) = True : genList xs
genList _ = []

inputList = genList input

dragonStep a = a ++ False : b
  where b = map not (reverse a)

diskLength1 = 272
diskLength2 = 35651584
diskLength = diskLength2

checkSum' (x:y:xs) = (x == y) : checkSum' xs
checkSum' _ = []

checkSum :: [Bool] -> [Bool]
checkSum = until (odd . length) checkSum' . checkSum'

finalData = take diskLength $ until ((>= diskLength) . length) dragonStep inputList
finalChecksum = checkSum finalData

prettify (True:xs) = '1' : prettify xs
prettify (False:xs) = '0' : prettify xs
prettify _ = []

main = putStrLn . prettify $ finalChecksum

