{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Monoid ((<>))
import Crypto.Hash.MD5 (hash)
import qualified  Data.ByteString.Char8 as B
import Data.String (fromString)
import Numeric (showHex)
import Data.ByteString.Base16 (encode)
import Control.Parallel.Strategies (withStrategy, parBuffer, r0)
import GHC.Conc (numCapabilities)
import Data.Array
import Data.Maybe (isJust, fromJust)

initial :: B.ByteString
initial = "ojvtpuvg"

keys :: [B.ByteString]
keys = filter ((== "00000") . B.take 5) .
  withStrategy (parBuffer numCapabilities r0) .
  map (
    encode . hash . (initial <>) . fromString . show
    ) $ [0..]



ugh :: Array Int (Maybe Char) -> [B.ByteString] -> [Char]
ugh a (b:bs) = let
    pos = read . (:[]) . B.head . B.drop 5 $ b
    val = B.head . B.drop 6 $ b
  in
    if (== 8) . length . filter isJust . elems $ a then
      map fromJust . elems $ a
    else case a ! pos of
      Just _ -> ugh a bs
      Nothing -> ugh (a // [(pos, Just val)]) bs


filterInvalidPos = filter (not . (`elem` (['8', '9'] <> ['a'..'f'])) . B.head . B.drop 5)

main = main2
main1 = putStrLn . map (B.head . B.drop 5) $ keys
main2 = putStrLn . ugh (listArray (0, 7) $ replicate 8 Nothing) . filterInvalidPos $ keys

