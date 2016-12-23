{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import Data.Char (isLower)
import qualified Data.Array as A
import Data.Maybe (mapMaybe)
import Control.Monad (join)

data Instruction = Copy Char Char
                 | CopyVal Int Char
                 | Inc Char
                 | Dec Char
                 | Jump Char Int
                 | JumpVal Int Int
  deriving (Show, Eq)


regParser :: Parser Char
regParser = satisfy isLower

copyParser :: Parser Instruction
copyParser = do
  string "cpy "
  regFrom <- regParser
  char ' '
  regTo <- regParser
  return $ Copy regFrom regTo

copyValParser :: Parser Instruction
copyValParser = do
  string "cpy "
  val <- signed decimal
  char ' '
  regTo <- regParser
  return $ CopyVal val regTo

incParser :: Parser Instruction
incParser = do
  string "inc "
  Inc <$> regParser

decParser :: Parser Instruction
decParser = do
  string "dec "
  Dec <$> regParser

jumpParser :: Parser Instruction
jumpParser = do
  string "jnz "
  reg <- regParser
  char ' '
  jumpLen <- signed decimal
  return $ Jump reg jumpLen

jumpValParser :: Parser Instruction
jumpValParser = do
  string "jnz "
  val <- signed decimal
  char ' '
  jumpLen <- signed decimal
  return $ JumpVal val jumpLen

instructionParser = choice
  [ copyParser
  , copyValParser
  , incParser
  , decParser
  , jumpParser
  , jumpValParser
  ]


data MachineState = MachineState
  { instructions :: A.Array Int Instruction
  , registers :: A.Array Char Int
  , currentInstructionPtr :: Int
  }

instance Show MachineState where
  show (MachineState instrs regs ptr) =
    "MachineState: (" ++ show (A.elems instrs) ++ ", " ++ show regs ++ ", " ++ show ptr ++ ")"


initialState :: [Instruction] -> MachineState
initialState instrs = MachineState instrArray regs 0
  where
    regs1 = A.listArray ('a', 'd') $ repeat 0
    regs2 = regs1 A.// [('c', 1)]
    regs = regs2
    instrArray = A.listArray (0, length instrs -1) instrs

exec :: MachineState -> MachineState
exec m = execInstr instr m
  where instr = instructions m A.! currentInstructionPtr m

execInstr :: Instruction -> MachineState -> MachineState
execInstr (Copy regFrom regTo) (MachineState instrs regs curr) =
  MachineState instrs (regs A.// [(regTo, regs A.! regFrom)]) (curr+1)
execInstr (CopyVal val reg) (MachineState instrs regs curr) =
  MachineState instrs (regs A.// [(reg, val)])  (curr+1)
execInstr (Inc reg) (MachineState instrs regs curr) =
  MachineState instrs (regs A.// [(reg, regs A.! reg +1)]) (curr+1)
execInstr (Dec reg) (MachineState instrs regs curr) =
  MachineState instrs (regs A.// [(reg, regs A.! reg -1)]) (curr+1)
execInstr (Jump c j) (MachineState instrs regs curr) =
  if (regs A.! c) == 0 then
    MachineState instrs regs (curr + 1)
  else
    MachineState instrs regs (curr + j)
execInstr (JumpVal v j) (MachineState instrs regs curr) =
  if v == 0 then
    MachineState instrs regs (curr + 1)
  else
    MachineState instrs regs (curr + j)


foldShit :: MachineState -> MachineState
foldShit m@(MachineState instrs _ curr) =
  if not (A.inRange (A.bounds instrs) curr) then
    m
  else
    foldShit . exec $ m

execToCompletion :: [Instruction] -> MachineState
execToCompletion = foldShit . initialState

maybeParser :: Parser a -> T.Text -> Maybe a
maybeParser parser = maybeResult . flip feed "". parse parser

main = join $ print . registers . execToCompletion .  mapMaybe (maybeParser instructionParser) . T.lines <$> T.getContents