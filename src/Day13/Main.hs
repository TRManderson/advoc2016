module Main where
import Data.Bits
import Control.Parallel.Strategies (parList, parListChunk, using, withStrategy, rdeepseq)
import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.Graph.Inductive hiding (empty)
import Data.Graph.Inductive.PatriciaTree (Gr(..))
import Data.Graph.Inductive.Query.SP (spLength)

problemInput = 1358 :: Int

isWall x y = isOdd || isOutside
  where
    isOutside = x > constraint +1 || y > constraint +1 || x < 0 || y < 0
    isOdd = odd . popCount $ poly + problemInput
    poly = x*x + 3*x + 2*x*y + y + y*y

validMoves x y = filter (not . uncurry isWall) allMoves
  where
    allMoves =
      [ (x+1, y)
      , (x-1, y)
      , (x, y+1)
      , (x, y-1)
      ]

constraint = 100
constrainedCells = withStrategy (parList rdeepseq) $ do
  x <- [0..constraint + 1]
  y <- [0..constraint + 1]
  if isWall x y then [] else [(x, y)]

problemNodeList :: [LNode (Int, Int)]
problemNodeList = zip [0..] constrainedCells

problemNodeMap :: M.Map (Int, Int) Node
problemNodeMap = M.fromList $ map (\(a,b) -> (b,a)) problemNodeList

problemEdges :: [LEdge Int]
problemEdges = withStrategy (parList rdeepseq) $ do
  cell <- constrainedCells
  let sourceNode = problemNodeMap M.! cell
  resultCell <- uncurry validMoves cell
  if (\(a, b) -> a <= constraint && b <= constraint) resultCell then do
      let targetNode = problemNodeMap M.! resultCell
      return (sourceNode, targetNode, 1)
  else
      []

graph :: Gr (Int, Int) Int
graph = mkGraph problemNodeList problemEdges

startNode = problemNodeMap M.! (1, 1)
endNode = problemNodeMap M.! (31, 39)

main1 = print $ spLength startNode endNode graph
main2 = print . filter ((<= 50) . length . unLPath) . spTree startNode $ graph

main = main2
