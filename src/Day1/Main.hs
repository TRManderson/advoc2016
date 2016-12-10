module Main where
import Data.Monoid ((<>))
import Data.List.Split (splitOn )
import Data.List (foldl', scanl')
import Data.Set (empty, insert, member, Set)
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

data Instruction = L Int | R Int deriving (Show, Eq)

magnitude :: Instruction -> Int
magnitude (L x) = x
magnitude (R x) = x

parse :: String -> [Instruction]
parse = fmap toInstruction . splitOn ", "
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

step :: (X, Y, Direction) -> Instruction -> [(X, Y, Direction)]
step (x, y, dir) instr = tail $ step (x, y)
  where
    newDir = changeDirection instr dir
    (newX, newY) = moveInDirection newDir (x, y) $ magnitude instr
    step (x, y) = if x == newX && y == newY then
      [(newX, newY, newDir)]
    else
      (x, y, newDir):step (moveInDirection newDir (x, y) 1)


main1 :: [Instruction] -> IO ()
main1 problem = do
  let (x, y, _) = last $ foldl' (\l i -> step (last l) i) [(0, 0, North)] problem
  putStrLn $ "result: " <> show (abs x + abs y)

next :: (X, Y) -> StateT (Set (X, Y), Maybe (X, Y)) IO Bool
next loc = do
  (s, found) <- get
  --liftIO $ print s
  case found of
    Just _ -> return True
    Nothing ->
      if loc `member` s then do
        put (s, Just loc)
        return True
      else do
        put (loc `insert` s, Nothing)
        return False

reduceLoc :: (X, Y, Direction) -> (X, Y)
reduceLoc (x, y, _) = (x, y)

main2 :: [Instruction] -> IO ()
main2 problem = do
  let steps = foldl' (\l i -> l <> step (last l) i) [(0, 0, North)] problem
  let statesListed = fmap (next . reduceLoc) steps
  let stated = foldl' (>>) (return False) statesListed
  possible <- snd <$> execStateT stated (empty, Nothing)
  case possible of
    Just (x, y) -> print (abs x + abs y)
    Nothing     -> putStrLn "No duplicates found"

main = parse <$> getLine >>= main2
