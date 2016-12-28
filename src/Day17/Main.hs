{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Monoid ((<>))
import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString.Char8 as B
import Data.String (fromString)
import Data.ByteString.Base16 (encode)
import GHC.Conc (numCapabilities)
import Control.Parallel.Strategies

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
step' (history, (posX, posY)) = filter (invalid . snd) . map each $ possible
  where
    possible = possiblePaths history
    dir 'U' = (posX, posY + 1)
    dir 'D' = (posX, posY - 1)
    dir 'L' = (posX - 1, posY)
    dir 'R' = (posX + 1, posY)
    each c = (history `B.snoc` 'U', dir c)
    invalid (x,y) = x> 3 || y > 3 || x < 0 || y < 0


step :: [(B.ByteString, (Int, Int))] -> [(B.ByteString, (Int, Int))]
step = withStrategy (parList rdeepseq) . (=<<) step'

initial = [(B.empty, (0, 0))]

solved = (==) (4,4) . snd

answer = head . filter solved . until (any solved) step $ initial

main = B.putStrLn . fst $ answer