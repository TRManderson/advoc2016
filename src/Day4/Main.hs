{-# LANGUAGE Safe #-}
module Main where
import Data.List
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Monad (join)
import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)

data Room = Room { names :: [String]
                 , roomId :: Int
                 , checkSum :: String
                 }
  deriving (Show, Eq)


sortOrder :: (Char, Int) -> (Int, Int)
sortOrder (c, i) =  (i, negate $ ord c)

valid :: Room -> Bool
valid r = checkSum r == (take 5 . map fst) nameChars
  where nameChars = reverse . sortOn sortOrder . map (head &&& length) . group . sort . join . names $ r

readNameCharsP :: ReadP [String]
readNameCharsP = many1 $ do
  inner <- many1 $ satisfy isLower
  char '-'
  return inner

readSectorP :: ReadP Int
readSectorP = read <$> many1 (satisfy isDigit)

readChecksumP :: ReadP String
readChecksumP = do
  char '['
  chars <- many1 $ satisfy isLower
  char ']'
  return chars

readRoomP :: ReadP Room
readRoomP = do
  names <- readNameCharsP
  sector <- readSectorP
  checksum <- readChecksumP
  return (Room names sector checksum)

roomParser :: String -> Maybe Room
roomParser s = case uncons.sortOn (length.snd) $ readP_to_S readRoomP s of
  Just ((a, _), _) -> Just a
  Nothing -> Nothing

parseInput :: String -> [Room]
parseInput = mapMaybe roomParser . lines

aggregate :: [Room] -> Int
aggregate = sum . map roomId

decrypt :: Room -> (String, Int)
decrypt = inner &&& roomId
  where
    shift q = chr . (+ ord 'a') . (`mod` 26) . (+q) . flip (-) (ord 'a') . ord
    inner (Room names roomId _) = intercalate "-" $ (fmap.fmap) (shift roomId) names


main = join $ traverse id . map (print . decrypt) . filter valid . parseInput <$> getContents