module Main where

data Element = Curium | Cobalt | Promethium | Ruthenium | Plutonium deriving (Eq, Ord, Show)
data Item = Generator Element | Microchip Element deriving (Eq, Ord, Show)

strings =
  [ "The first floor contains a promethium generator and a promethium-compatible microchip."
  , "The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator."
  , "The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip."
  , "The fourth floor contains nothing relevant."
  ]

initialState :: [[Item]]
initialState =
  [ [Generator Promethium, Microchip Promethium]
  , [Generator Cobalt, Generator Curium, Generator Ruthenium, Generator Plutonium]
  , [Microchip Cobalt, Microchip Curium, Microchip Ruthenium, Microchip Plutonium]
  , []
  ]

main = traverse print initialState