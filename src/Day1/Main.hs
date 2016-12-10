module Main where
import Data.Monoid ((<>))
import Data.List.Split (splitOn )
import Data.List (foldl', scanl')

data Instruction = L Int | R Int deriving (Show, Eq)

magnitude :: Instruction -> Int
magnitude (L x) = x
magnitude (R x) = x

parse :: String -> [Instruction]
parse = map toInstruction . splitOn ", "
  where
    toInstruction ('L':xs) = L (read xs)
    toInstruction ('R':xs) = R (read xs)
    toInstruction _ = undefined

type X = Int
type Y = Int
data Direction = North | East | South | West deriving (Enum, Show, Eq)

changeDirection :: Instruction -> Direction -> Direction
changeDirection (L _) = toEnum . (`mod` 4) . (+3) . fromEnum
changeDirection (R _) = toEnum . (`mod` 4) . (+1) . fromEnum

moveInDirection :: Direction -> (X, Y) -> Int -> (X, Y)
moveInDirection North (a, b) d = (a, b+d)
moveInDirection South (a, b) d = (a, b-d)
moveInDirection East (a, b) d = (a+d, b)
moveInDirection West (a, b) d = (a-d, b)

step :: (X, Y, Direction) -> Instruction -> (X, Y, Direction)
step (x, y, dir) instr = (newX, newY, newDir)
  where
    newDir = changeDirection instr dir
    (newX, newY) = moveInDirection newDir (x, y) $ magnitude instr


main :: IO ()
main = do
  problem <- parse <$> getLine
  let steps = scanl' step (0, 0, North) problem
  traverse print $ zip problem steps
  let (x, y, _) = last steps
  putStrLn $ "result: " <> show (abs x + abs y)