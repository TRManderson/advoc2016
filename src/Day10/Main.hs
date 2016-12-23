{-# LANGUAGE OverloadedStrings#-}
module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text hiding (match)
import Data.Maybe
import Control.Monad
import Data.List (nub, sort, sortOn, uncons)
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.Async

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

maybeParser :: Parser a -> T.Text -> Maybe a
maybeParser p = maybeResult . flip feed "" . parse p

extractLocations :: Transition -> [Location]
extractLocations (Transition a (Give b)) = [a, b]
extractLocations (Transition a (Compare b c)) = [a, b, c]

type Channel = Chan Int
execTransition :: M.Map Location Channel -> Transition -> IO ()
execTransition locChans (Transition from (Give to)) = do
  let inputChan = locChans M.! from
      outputChan = locChans M.! to
  readChan inputChan >>= writeChan outputChan
execTransition locChans (Transition from (Compare lower upper)) = do
  let inputChan = locChans M.! from
      lowerChan = locChans M.! lower
      upperChan = locChans M.! upper
  [lowerVal, upperVal] <- sort <$> sequence [readChan inputChan, readChan inputChan]
  when ([lowerVal, upperVal] == [17, 61]) $ print from
  writeChan upperChan upperVal
  writeChan lowerChan lowerVal

mkChannels :: [Location] -> IO (M.Map Location Channel)
mkChannels locs = M.fromList <$> traverse (\a -> (,) a <$> newChan) locs

startGraph :: M.Map Location Channel -> IO ()
startGraph chanMap = forM_ (M.keys chanMap) $ \loc ->
  case loc of
    Value x -> writeChan (chanMap M.! loc) x
    _ -> return ()

finishGraph :: M.Map Location Channel -> IO [Async  ()]
finishGraph chanMap = forM (M.keys chanMap) $ \loc ->
  async $ case loc of
    Output x -> when (x <= 2) $ do
      val <- readChan (chanMap M.! loc)
      putStrLn $ "Output " ++ show x ++ ": " ++ show val
    _ -> return ()

main = do
  transitions <- mapMaybe (maybeParser parser) . T.lines <$> T.getContents
  let locations = nub $ transitions >>= extractLocations
  chanMap <- mkChannels locations
  startGraph chanMap
  actions <- traverse (async . execTransition chanMap) transitions
  outputs <- finishGraph chanMap
  traverse wait (actions ++ outputs)
  return ()
