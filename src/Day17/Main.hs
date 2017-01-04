{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Monoid ((<>))
import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString.Char8 as B
import Data.String (fromString)
import Data.ByteString.Base16 (encode)
import GHC.Conc (numCapabilities)
import Control.Parallel.Strategies
import Data.List (maximumBy)
import Data.Ord (comparing)

password = "qtetzkpl" :: B.ByteString
unlocked c
  | c >= 'b' = True
  | c < 'b'  = False
  | otherwise = error "Unexpected character"


possiblePaths pathHistory = map fst . filter snd $ zip ['U', 'D', 'L', 'R'] [u,d,l,r]
  where
    hashed = encode . hash . B.append password $ pathHistory
    [u,d,l,r] = map (unlocked . B.index hashed) [0..3]

step' :: (B.ByteString, (Int, Int)) -> [(B.ByteString, (Int, Int))]
step' state@(history, (posX, posY)) = if solved state then [state] else
    filter (not . invalid . snd) . map each $ possible
  where
    possible = possiblePaths history
    dir 'U' = (posX, posY - 1)
    dir 'D' = (posX, posY + 1)
    dir 'L' = (posX - 1, posY)
    dir 'R' = (posX + 1, posY)
    each c = (history `B.snoc` c, dir c)
    invalid (x,y) = x> 3 || y > 3 || x < 0 || y < 0


step :: [(B.ByteString, (Int, Int))] -> [(B.ByteString, (Int, Int))]
step = withStrategy (parList rdeepseq) . (=<<) step'

initial = [(B.empty, (0, 0))]

solved = (==) (3,3) . snd

answer1 = fst . head . filter solved . until (any solved) step $ initial

answer2' = maximumBy (comparing B.length) . map fst . filter solved . until (all solved) step $ initial
answer2 = fromString . show . B.length $ answer2'

answer = answer2

main = B.putStrLn answer