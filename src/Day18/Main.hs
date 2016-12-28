module Main where
import Control.Monad (join)

parse ('.':xs) = False : parse xs
parse ('^':xs) = True : parse xs
parse _ = []

preds :: [Bool -> Bool -> Bool -> Bool]
preds =
  [ \a b c -> a && b && not c
  , \a b c -> not a && b && c
  , \a b c -> a && not b && not c
  , \a b c -> not a && not b && c
  ]

apply :: Bool -> Bool -> Bool -> (Bool -> Bool -> Bool -> Bool) -> Bool
apply a b c f = f a b c

nextRow' (a:b:c:xs) = any (apply a b c) preds : nextRow' (b:c:xs)
nextRow' _ = []
nextRow xs = nextRow' $ False : (xs ++ [False])

allRows = take numRows . iterate nextRow

pretty = map pretty'
  where pretty' False = '.'
        pretty' True  = '^'

main = join $ print . length . filter not . concat . allRows . parse <$> getLine

numRows1 = 40
numRows2 = 400000
numRows = numRows2