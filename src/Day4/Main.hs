module Main where
import Data.List
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Monad (join)
import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)

data Room = Room { nameChars :: [(Char, Int)]
                 , roomId :: Int
                 , checkSum :: String
                 }
  deriving (Show, Eq)


sortOrder :: (Char, Int) -> (Int, Int)
sortOrder (c, i) =  (i, negate $ ord c)

valid :: Room -> Bool
valid r = checkSum r == (take 5 . map fst . nameChars  $ r)

readNameCharsP :: ReadP [(Char, Int)]
readNameCharsP = do
  chars <- many1 $ do
    inner <- many1 $satisfy isLower
    char '-'
    return inner
  return . reverse . sortOn sortOrder . map (head &&& length) . group . sort . join $ chars

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

main = join $ print . aggregate . filter valid . parseInput <$> getContents