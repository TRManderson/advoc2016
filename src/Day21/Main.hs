{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import qualified Data.Sequence as S
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad
import Data.Foldable (toList, foldl')

data Instruction = SwapPos Int Int
                 | SwapLetter Char Char
                 | RotateLeft Int
                 | RotateRight Int
                 | RotatePos Char
                 | Reverse Int Int
                 | Move Int Int
                 deriving (Eq, Show)

genParser1 :: (T.Text, Parser a) -> (a -> b) -> Parser b
genParser1 (str, parseVal) constructor = do
  string str
  val <- parseVal
  return $ constructor val
genParser2 :: (T.Text, Parser a) -> (T.Text, Parser b) -> (a -> b -> c) -> Parser c
genParser2  p1 (str, parseVal) constructor = do
  cons' <- genParser1 p1 constructor
  string str
  val <- parseVal
  return $ cons' val

parseSwapPos :: Parser Instruction
parseSwapPos = genParser2 ("swap position ", decimal) (" with position ", decimal) SwapPos

parseSwapLetter :: Parser Instruction
parseSwapLetter = genParser2 ("swap letter ", letter) (" with letter ", letter) SwapLetter

parseRotateLeft :: Parser Instruction
parseRotateLeft = choice
  [ genParser1 ("rotate left ", decimal) RotateLeft <* string " steps"
  , string "rotate left 1 step" >> return (RotateLeft 1)
  ]

parseRotateRight :: Parser Instruction
parseRotateRight = choice
  [ genParser1 ("rotate right ", decimal) RotateRight <* string " steps"
  , string "rotate right 1 step" >>  return (RotateRight 1)
  ]

parseRotatePos :: Parser Instruction
parseRotatePos = genParser1 ("rotate based on position of letter ", letter) RotatePos

parseReverse :: Parser Instruction
parseReverse = genParser2 ("reverse positions ", decimal) (" through ", decimal) Reverse

parseMove :: Parser Instruction
parseMove = genParser2 ("move position ", decimal) (" to position ", decimal) Move

parser = choice
  [ parseSwapPos
  , parseSwapLetter
  , parseRotateLeft
  , parseRotateRight
  , parseRotatePos
  , parseReverse
  , parseMove
  ]

maybeParser :: Parser a -> T.Text -> Maybe a
maybeParser p = maybeResult . flip feed "" . parse p

apply :: Instruction -> S.Seq Char -> S.Seq Char
apply (SwapPos a b) seq = fromJust $ do
  charA <- seq S.!? a
  charB <- seq S.!? b
  return . S.update b charA . S.update a charB $ seq
apply (SwapLetter a b) seq = fmap swapFn seq
  where swapFn c
          | c == a = b
          | c == b = a
          | otherwise = c
apply (RotateLeft n) seq = stablePart S.>< movedPart
  where shift = n `mod` S.length seq
        movedPart = S.take shift seq
        stablePart = S.drop shift seq
apply (RotateRight n) seq = movedPart S.>< stablePart
  where size = S.length seq
        movedPart = S.drop (size - (n `mod` size)) seq
        stablePart = S.take (size - (n `mod` size)) seq
apply (RotatePos c) seq =  flip apply seq . RotateRight . (\n -> if n >= 4 then n+2 else n+1) . fromJust . S.elemIndexL c $ seq
apply (Reverse from to) seq = pre S.>< mid S.>< post
  where pre  = S.take from seq
        post = S.drop (to + 1) seq
        mid  = S.reverse . S.drop from . S.take (to + 1) $ seq
apply (Move from to) seq = fromJust $ do
  val <- seq S.!? from
  return . S.insertAt to val . S.deleteAt from $ seq

unapply v@(SwapLetter _ _) = apply v
unapply (RotateLeft n) = apply (RotateRight n)
unapply (RotateRight n) = apply (RotateLeft n)
unapply v@(SwapPos _ _) = apply v
unapply (Move from to) = apply (Move to from)
unapply (RotatePos c) = unRotatePos c
unapply v@(Reverse _ _) = apply v

unRotatePos :: Char -> S.Seq Char -> S.Seq Char
unRotatePos c seq = apply (RotateLeft $ mv idx) seq
  where idx = fromJust $ S.elemIndexL c seq
        mv 1 = 1
        mv 3 = 2
        mv 5 = 3
        mv 7 = 4
        mv 2 = 6
        mv 4 = 7
        mv 6 = 0
        mv 0 = 1

main = do
  input <- mapMaybe (maybeParser parser . T.pack) . lines <$> getContents
  let soln1 = foldl' (flip apply) "abcdefgh" input
  let soln2 = foldr unapply "fbgdceah" input
  putStrLn . toList $ soln1
  putStrLn . toList $ soln2

