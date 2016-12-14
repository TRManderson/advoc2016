{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Main where
import Data.Attoparsec.Text
import Data.Array -- gets me Data.Ix for free
import Data.Maybe (mapMaybe)
import Control.Monad (join)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable (fold, foldl', foldr1)

data Instruction = Rect Int Int
                 | RotateRow Int Int
                 | RotateCol Int Int
                 deriving (Show, Eq)

genParser :: T.Text -> T.Text -> (Int -> Int -> a) -> Parser a
genParser first mid cons = do
  string first
  a <- decimal
  string mid
  b <- decimal
  endOfInput
  return (cons a b)

parseRect :: Parser Instruction
parseRect = genParser "rect " "x" (flip Rect)
parseRotateRow :: Parser Instruction
parseRotateRow = genParser "rotate row y=" " by " RotateRow
parseRotateCol :: Parser Instruction
parseRotateCol = genParser "rotate column x=" " by " RotateCol

parser :: Parser Instruction
parser = choice [ parseRect
                , parseRotateRow
                , parseRotateCol
                ]

parseInstruction :: T.Text -> Maybe Instruction
parseInstruction = maybeResult . flip feed "" . parse parser

initial :: Array (Int, Int) Bool
initial = listArray ((0,0), (5,49)) . map (const False) . range $ ((0,0), (5,49))

flip' :: Ix i => i -> Array i Bool -> Array i Bool
flip' idx arr = arr // [(idx, val)]
  where val = not (arr ! idx)

rotateRow :: Int -> Int -> Array (Int, Int) a -> Array (Int, Int) a
rotateRow qty row arr = arr // zipWith shifter idxs shifted
  where
    ((_, minCol), (_, maxCol)) = bounds arr
    idxs = [(row, col) | col <- [minCol..maxCol]]
    shifted = [(row, (col + qty) `mod` (maxCol+1)) | col <- [minCol..maxCol]]
    shifter oldPos newPos = (newPos, arr ! oldPos)

rotateCol :: Int -> Int -> Array (Int, Int) a -> Array (Int, Int) a
rotateCol qty col arr = arr // zipWith shifter idxs shifted
  where
    ((minRow, _), (maxRow, _)) = bounds arr
    idxs = [(row, col) | row <- [minRow..maxRow]]
    shifted = [((row + qty) `mod` (maxRow+1), col) | row <- [minRow..maxRow]]
    shifter oldPos newPos = (newPos, arr ! oldPos)

runInstruction :: Instruction -> Array (Int, Int) Bool -> Array (Int, Int) Bool
runInstruction (Rect a b) = \arr -> arr // zip (range ((0,0), (a-1,b-1))) (repeat True)
runInstruction (RotateRow a b) = rotateRow b a
runInstruction (RotateCol a b) = rotateCol b a

main' :: ([Instruction] -> IO ()) -> IO ()
main' transform = join $ transform . mapMaybe parseInstruction . T.lines <$> T.getContents

executeInstructions :: [Instruction] -> Array (Int, Int) Bool
executeInstructions = foldl' (flip runInstruction) initial

main1 = print . length . filter id . elems . executeInstructions

main2 = putStrLn . prettyPrint . executeInstructions

prettyPrint :: Array (Int, Int) Bool -> String
prettyPrint arr = unlines chars
  where
    toChars True = '█'
    toChars False = ' '
    charArr = fmap toChars arr
    ((minRow, minCol), (maxRow, maxCol)) = bounds arr
    idxs = [[(row, col) | col <- [minCol..maxCol]] | row <- [minRow..maxRow]]
    chars = (fmap.fmap) (charArr !) idxs


main = main' main2
