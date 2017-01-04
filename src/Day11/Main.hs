module Main where
import Control.Parallel.Strategies
import Data.List (partition)
data Element = Curium | Cobalt | Promethium | Ruthenium | Plutonium deriving (Eq, Ord, Show)
data Floor = Floor { getGenerators :: [Element]
                   , getMicrochips :: [Element]
                   } deriving (Eq, Show)

instance NFData Element
instance NFData Floor where
  rnf (Floor a b) = a `seq` b `seq` ()

strings =
  [ "The first floor contains a promethium generator and a promethium-compatible microchip."
  , "The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator."
  , "The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip."
  , "The fourth floor contains nothing relevant."
  ]

initialFloorState :: [Floor]
initialFloorState =
  [ Floor [Promethium] [Promethium]
  , Floor [Cobalt, Curium, Ruthenium, Plutonium] []
  , Floor [] [Cobalt, Curium, Ruthenium, Plutonium]
  , Floor [] []
  ]

data FloorState = FS Floor Floor Floor Floor
instance NFData FloorState where
  rnf (FS a b c d) = foldr seq () [a,b,c,d]
getFloor 1 (FS a _ _ _) = a
getFloor 2 (FS _ b _ _) = b
getFloor 3 (FS _ _ c _) = c
getFloor 4 (FS _ _ _ d) = d

type CurrentFloor = Int
type Steps = Int
data State = S FloorState CurrentFloor Steps

instance NFData State where
  rnf (S f c s) = rnf f `seq` c `seq` s `seq` ()

getSteps (S _ _ n) = n

step :: State -> [State]
step' (S floors currentFloor steps) = undefined

step :: [State] -> [State]
step = withStrategy (parList rdeepseq) . (=<<) step'

floorValid :: Floor -> Bool
floorValid (Floor gens chips) = False

finalFloor = let allElems = [Promethium, Cobalt, Curium, Ruthenium, Plutonium] in
  Floor allElems allElems
emptyFloor = Floor [] []

finalState = S [emptyFloor, emptyFloor, emptyFloor, finalFloor] 4
isDone :: State -> Bool
isDone = (==) finalState

initalState = S initialFloorState 1 0

answer = getSteps . head . filter isDone . until (any isDone) step initalState

main = print answer