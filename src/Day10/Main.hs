{-# LANGUAGE OverloadedStrings#-}
module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text hiding (match)
import Data.Maybe
import Control.Monad
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (topsort)
import Data.List (nub, sort, sortOn, uncons)
import qualified Data.Map as M
import Control.Monad.Trans.State

data Location = Value Int | Bot Int | Output Int deriving (Ord, Show, Eq)
data Transition = Give Location Location | Compare Location Location Location deriving (Show, Eq, Ord)

genLocParser :: T.Text -> (Int -> a) -> Parser a
genLocParser t c = c <$> decimal << string t

parseLocation :: Parser Location
parseLocation = choice [ genLocParser "bot " Bot
                       , genLocParser "value " Value
                       , genLocParser "output " Output
                       ]

parseGive :: Parser Transition
parseGive = do
  loc1 <- parseLocation
  string " goes to "
  loc2 <- parseLocation
  return $ Give loc1 loc2

parseCompare :: Parser Transition
parseCompare = do
  loc1 <- parseLocation
  string " gives low to "
  loc2 <- parseLocation
  string " and high to "
  loc3 <- parseLocation
  return $ Compare loc1 loc2 loc3

parser :: Parser Transition
parser = choice [ parseCompare
                , parseGive
                ]

extractLocations :: Transition -> [Location]
extractLocations (Give a b) = [a, b]
extractLocations (Compare a b c) = [a, b, c]

makeContext :: M.Map Location Int -> Transition -> [Context Location Ordering]
makeContext m (Give a b) =
  [ ([(EQ, m M.! a)], m M.! b, b, [])
  , ([], m M.! a, a, [])
  ]
makeContext m (Compare from toLow toHigh) =
  [ ([], m M.! from, from, [(LT, m M.! toLow), (GT, m M.! toHigh)])
  , ([], m M.! toLow, toLow, [])
  , ([], m M.! toHigh, toHigh, [])
  ]

makeGraph :: DynGraph g => [Transition] -> g Location Ordering
makeGraph transitions = buildGr contexts
  where
    locations = transitions >>= sort . nub . extractLocations
    locMap = M.fromList $ zip locations [1..]
    contexts = transitions >>= makeContext locMap

readGraph :: T.Text -> Gr Location Ordering
readGraph = makeGraph . mapMaybe (maybeResult . flip feed "" . parse parser) . T.lines


type IntermediateSolution = M.Map Location (Maybe Int, Maybe Int)


reduceG :: Gr Location Ordering -> IntermediateSolution
reduceG gr = execState joinedStates
  where
    sortedNodes = topsort gr
    joinedStates = traverse toState contexts
    toState node = do
      let (Just (_, _, loc, forward), _) = match node gr
      m <- get



reduceI :: IntermediateSolution -> Maybe Location
reduceI = fst . uncons . map fst . filter ((== (17, Just 61)) . snd) . M.toList

main = T.getContents >>= print . reduceI . reduceG . readGraph

