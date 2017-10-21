{-|
Module      : EagleTree
Description : Tool for accessing EagleTree logs.
Copyright   : (c) Dustin Sallings, 2017
Maintainer  : dustin@spy.net
Stability   : experimental

-}

module EagleTree
    ( parseLog
    , Session
    -- * Session Value Accessors
    -- $valueAccessors
    , column
    , intColumn
    , floatColumn
    , colNames
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

instance Show Session where
  show (Session n cn cv) =
    n ++ " cols=" ++ show cn ++ ", " ++ show (length cv) ++ " readings"

-- | Return the values of a named column while performing arbitrary
-- conversion on the input.
--
-- See `intColumn` and `floatColumn` for common use cases.
column :: (String -> t) -> String -> Session -> [t]
column f name (Session _ names vals) =
  case Map.lookup name names of
    Nothing -> []
    Just x -> map (f . (!! x) . words) vals

-- | Return the values of a named column as ints.
intColumn :: String -> Session -> [Int]
intColumn = column read

-- | Return the values of a named column as floats.
floatColumn :: String -> Session -> [Float]
floatColumn = column read

-- | Retrieve a list of all possible column names.
colNames :: Session -> [String]
colNames = Map.keys.colNames_

-- | Parse a log from a `BL.ByteString`.
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
