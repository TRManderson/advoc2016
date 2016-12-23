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
import Data.Graph.Inductive.Dot

data Location = Value Int | Bot Int | Output Int deriving (Ord, Show, Eq)
data Endpoint = Give Location | Compare Location Location
data Transition = Transition Location Endpoint

genLocParser :: T.Text -> (Int -> a) -> Parser a
genLocParser t c = c <$> (string t >> decimal)

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
  return $ Transition loc1 (Give loc2)

parseCompare :: Parser Transition
parseCompare = do
  loc1 <- parseLocation
  string " gives low to "
  loc2 <- parseLocation
  string " and high to "
  loc3 <- parseLocation
  return $ Transition loc1 (Compare loc2 loc3)

parser :: Parser Transition
parser = choice [ parseCompare
                , parseGive
                ]

extractLocations :: Transition -> [Location]
extractLocations (Transition a (Give b)) = [a, b]
extractLocations (Transition a (Compare b c)) = [a, b, c]

makeContext :: M.Map Location Int -> Transition -> [Context Location Ordering]
makeContext m (Transition a (Give b)) =
  [ ([(EQ, m M.! a)], m M.! b, b, [])
  , ([], m M.! a, a, [])
  ]
makeContext m (Transition from (Compare toLow toHigh)) =
  [ ([], m M.! from, from, [(LT, m M.! toLow), (GT, m M.! toHigh)])
  , ([], m M.! toLow, toLow, [])
  , ([], m M.! toHigh, toHigh, [])
  ]

makeGraph :: DynGraph g => [Transition] -> g Location Ordering
makeGraph transitions = mkGraph nodes edges
  where
    locations = transitions >>= sort . nub . extractLocations
    locMap = M.fromList $ zip locations [1..]
    contexts = transitions >>= makeContext locMap
    nodes = nub . map (\(_, n, l, _) -> (n, l)) $ contexts
    edgesFromContext (pre, n, l, post) = mconcat
      [ map (\(label, from) -> (from, n, label)) pre
      , map (\(label, to) -> (n, to, label)) post
      ]
    edges = nub $ contexts >>= edgesFromContext

readGraph :: T.Text -> Gr Location Ordering
readGraph = makeGraph . mapMaybe (maybeResult . flip feed "" . parse parser) . T.lines

main = T.getContents >>= putStrLn . showDot . fglToDot . readGraph

