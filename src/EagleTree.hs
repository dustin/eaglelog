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
    , doubleColumn
    , ETGPSData(..)
    , gpsData
    , colNames
    , rows
    , ETRow
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)
import Data.List (zipWith7)

import Data.Csv (ToRecord(..), ToNamedRecord(..), namedRecord, record, (.=))

newtype SessionHeader = SessionHeader (Int, Int, String) deriving (Show)

data Session = Session { name :: String
                       , colNames_ :: [String]
                       , colMap_ :: Map.Map String Int
                       , colVals :: [String]
                       }

instance Show Session where
  show (Session n cn _ cv) =
    n ++ " cols=" ++ show cn ++ ", " ++ show (length cv) ++ " readings"

-- | Return the values of a named column while performing arbitrary
-- conversion on the input.
--
-- See `intColumn` and `floatColumn` for common use cases.
column :: (String -> t) -> String -> Session -> Either String [t]
column f name (Session _ _ cm vals) =
  case Map.lookup name cm of
    Nothing -> Left $ "invalid column name: " ++ name
    Just x -> Right $ map (f . (!! x) . words) vals

-- | Return the values of a named column as ints.
intColumn :: String -> Session -> Either String [Int]
intColumn = column read

-- | Return the values of a named column as floats.
floatColumn :: String -> Session -> Either String [Float]
floatColumn = column read

-- | Return the values of a named column as doubles.
doubleColumn :: String -> Session -> Either String [Double]
doubleColumn = column read

data ETGPSData = ETGPSData { gpsLat :: Double
                           , gpsLon :: Double
                           , gpsAlt :: Float
                           , gpsSpeed :: Float
                           , gpsCourse :: Float
                           , gpsDist :: Float
                           , gpsNumSats :: Int
                           } deriving (Show, Eq)

-- | Get relevant GPS data from the session.
gpsData :: Session -> [ETGPSData]
gpsData s@(Session _ _ _ vals) =
  zipWith7 ETGPSData (fdc "GPSLat") (fdc "GPSLon") (ffc "GPSAlt") (ffc "GPSSpeed")
                     (ffc "GPSCourse") (ffc "GPSDist") (fromRight $ intColumn "NumSats" s)
  where fdc = fromRight . flip doubleColumn s
        ffc = fromRight . flip floatColumn s
        fromRight (Left x) = error (show x)
        fromRight (Right x) = x

-- | Retrieve a list of all possible column names.
colNames :: Session -> [String]
colNames = colNames_

-- | Extract the individual rows from a session.
rows :: Session -> [ETRow]
rows s@(Session _ _ _ rs) = map (\r -> ETRow s r) rs

-- | A row from within a session.
data ETRow = ETRow Session String

bcp = map BC.pack

instance ToRecord ETRow where
    toRecord (ETRow _ s) = record $ (bcp $ words s)

instance ToNamedRecord ETRow where
    toNamedRecord (ETRow s r) = namedRecord (map (uncurry (.=)) $ zip (bcp $ colNames s) (bcp $ words r))

-- | Parse a log from a `BL.ByteString`.
parseLog :: BL.ByteString -> [Session]
parseLog f = let l = dropWhile (\l -> BL.unpack l /= "All Sessions") $ cleanLines f
                 (shdr, rest) = parseHeaders (drop 2 l)
                 names = map BL.unpack (BL.words $ head rest)
                 cm = Map.fromList $ zip names [0..]
                 readings = map BL.unpack $ tail rest in
               map (\ (SessionHeader (f, t, n)) -> Session n names cm $ take (t-f) . drop f $ readings) shdr
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
