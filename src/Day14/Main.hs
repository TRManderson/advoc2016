{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Monoid
import Crypto.Hash.MD5 (hash)
import qualified  Data.ByteString.Char8 as B
import Data.String (fromString)
import Control.Parallel.Strategies
import GHC.Conc (numCapabilities)
import Data.ByteString.Base16 (encode)
import Data.List (intersect, null)
import Data.List.Split (chunksOf)
import Control.Arrow
import Control.Monad

puzzleInput = "zpqevtbw"
hashes :: [(Int, B.ByteString)]
hashes = withStrategy (parBuffer (4*numCapabilities) rdeepseq) $
  map (\idx -> (idx, encode . hash . (puzzleInput <>) . fromString . show $ idx)) [0..]

reductionStrategy :: Int -> Strategy [a]
reductionStrategy n = parBuffer n rseq

reducer :: [(Int, B.ByteString)] -> [(Int, B.ByteString)]
reducer ((idx, x):xs) = result
  where
    groups = take 1 . map B.head . filter ((>= 3) . B.length) . B.group $ x
    others = take 1000 xs >>= (map B.head . filter ((>= 5) . B.length) . B.group . snd)
    result = if not . null . intersect groups $ others then
        (idx, x) : reducer xs
      else
        reducer xs

keys = withStrategy (reductionStrategy 4)$ reducer hashes

main = print . (!! 63) $ keys