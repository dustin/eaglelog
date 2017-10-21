module EagleTree
    ( parseLog
    , column
    , intColumn
    , floatColumn
    , colNames
    , Session
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)

newtype SessionHeader = SessionHeader (Int, Int, String) deriving (Show)

data Session = Session { name :: String
                       , colNames_ :: Map.Map String Int
                       , colVals :: [String]
                       }

column :: (String -> t) -> String -> Session -> [t]
column f name (Session _ names vals) =
  case Map.lookup name names of
    Nothing -> []
    Just x -> map (f . (!! x) . words) vals

intColumn :: String -> Session -> [Int]
intColumn = column read

floatColumn :: String -> Session -> [Float]
floatColumn = column read

colNames :: Session -> [String]
colNames = Map.keys.colNames_

instance Show Session where
  show (Session n cn cv) =
    n ++ " cols=" ++ show cn ++ ", " ++ show (length cv) ++ " readings"

parseLog :: BL.ByteString -> [Session]
parseLog f = let l = dropWhile (\l -> BL.unpack l /= "All Sessions") $ cleanLines f
                 (shdr, rest) = parseHeaders (drop 2 l)
                 fields = Map.fromList $ zip (map BL.unpack $ BL.words $ head rest) [0..]
                 readings = map BL.unpack $ tail rest in
               map (\ (SessionHeader (f, t, n)) -> Session n fields $ take (t-f) . drop f $ readings) shdr
  where
    parseHeaders :: [BL.ByteString] -> ([SessionHeader], [BL.ByteString])
    parseHeaders = let go h l@(a:b:rest) =
                         let nums = BL.words b
                             name = a in
                           if length nums == 2 then
                             go (SessionHeader(readbs nums 0, readbs nums 1, BL.unpack name):h) rest
                           else (reverse h, l) in
                     go []

    readbs :: Read a => [BL.ByteString] -> Int -> a
    readbs l i = read $ BL.unpack $ l !! i

    cleanLines = map (BL.filter ('\r' /=)) . BL.lines
