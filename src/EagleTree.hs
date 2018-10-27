{-# LANGUAGE OverloadedStrings #-}

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
    , gpsDatum
    ) where

import Control.DeepSeq (NFData(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map

import Data.Csv (ToRecord(..), ToNamedRecord(..), namedRecord, record, (.=))

newtype SessionHeader = SessionHeader (Int, Int, String) deriving (Show)

data Session = Session { _name :: String
                       , _colNames :: [BC.ByteString]
                       , _colMap :: Map.Map BC.ByteString Int
                       , _colVals :: [BL.ByteString]
                       }

instance Show Session where
  show (Session n cn _ cv) =
    n <> " cols=" <> show cn <> ", " <> show (length cv) <> " readings"

doubleTransform :: (Double -> Double) -> BL.ByteString -> BL.ByteString
doubleTransform f s = fromMaybe s $ (BL.pack . show . f) <$> (readMaybe . BL.unpack) s

-- Rewrite columns.
translate :: BC.ByteString -> (BL.ByteString -> BL.ByteString)
translate x
  | x == "Altitude" = feetToMeters
  | x == "GPSAlt" = feetToMeters
  | x == "GPSDist" = feetToMeters
  | x == "TempSensor" = f2c
  | x == "AmbientTemp" = f2c
  | x == "GPSSpeed" = mph2kph
  | x == "AirSpeed" = mph2kph
  | "*100" `BC.isSuffixOf` x = doubleTransform (/100)
  | "*10" `BC.isSuffixOf` x = doubleTransform (/10)
  | otherwise = id

  where
    feetToMeters = doubleTransform (* 0.3048)
    f2c = doubleTransform (\f -> (f - 32) * 5 / 9)
    mph2kph = doubleTransform (* 1.60934)

-- | Return the values of a named column while performing arbitrary
-- conversion on the input.
--
-- See `intColumn` and `floatColumn` for common use cases.
column :: (String -> t) -> String -> Session -> Either String [t]
column f name (Session _ _ cm vals) =
  case Map.lookup (BC.pack name) cm of
    Nothing -> Left $ "invalid column name: " <> name
    Just x -> Right $ map (f . BL.unpack . translate (BC.pack name) . (!! x) . BL.words) vals

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

instance NFData ETGPSData where
  rnf e@(ETGPSData la lo al sp c d n) =
    seq la seq lo seq al seq sp seq c seq d seq n seq e ()

-- | Get relevant GPS data from the session.
gpsData :: Session -> [ETGPSData]
gpsData = map gpsDatum . rows

-- | Retrieve a list of all possible column names.
colNames :: Session -> [String]
colNames = map (BC.unpack . colNameMap) . _colNames

-- | Extract the individual rows from a session.
rows :: Session -> [ETRow]
rows s@(Session _ _ _ rs) = map (ETRow s) rs

-- | A row from within a session.
data ETRow = ETRow Session BL.ByteString

bcw :: Session -> BL.ByteString -> [BC.ByteString]
bcw sess row = map (\(f,v) -> f v) $ zip (map t (_colNames sess)) $ BC.split ' ' . BL.toStrict $ row

  where
    t :: BC.ByteString -> (BC.ByteString -> BC.ByteString)
    t n = BL.toStrict . translate n . BL.fromStrict

instance ToRecord ETRow where
    toRecord (ETRow sess s) = record $ bcw sess s

colNameMap :: BC.ByteString -> BC.ByteString
colNameMap "File_Creation_Date_and_GPS_Local_Time" = "GPSTime"
colNameMap x = case BC.elemIndex '*' x of
                 Nothing -> x
                 Just n -> BC.take n x

instance ToNamedRecord ETRow where
    toNamedRecord (ETRow s r) = namedRecord (zipWith (.=) (map BC.pack $ colNames s) (bcw s r))

-- | Extra the GPS fields from a row.
gpsDatum :: ETRow -> ETGPSData
gpsDatum (ETRow (Session _ _ cm _) r) = ETGPSData (c "GPSLat") (c "GPSLon") (c "GPSAlt") (c "GPSSpeed")
                                                  (c "GPSCourse") (c "GPSDist") (c "NumSats")
  where w = (words . BL.unpack) r
        c s = case Map.lookup (BC.pack s) cm of
                Nothing -> error ("invalid column: " <> s)
                Just x -> read (w !! x)

-- | Parse a log from a `BL.ByteString`.
parseLog :: BL.ByteString -> [Session]
parseLog f = let l = dropWhile (\l' -> BL.unpack l' /= "All Sessions") $ cleanLines f
                 (shdr, rest) = parseHeaders (drop 2 l)
                 names = map BL.toStrict (BL.words $ head rest)
                 cm = Map.fromList $ zip names [0..]
                 readings = tail rest in
               map (\ (SessionHeader (f', t, n)) -> Session n names cm $ take (t-f') . drop f' $ readings) shdr
  where
    parseHeaders :: [BL.ByteString] -> ([SessionHeader], [BL.ByteString])
    parseHeaders = let go h l@(a:b:rest) =
                         let nums = BL.words b
                             name = a in
                           if length nums == 2 then
                             go (SessionHeader(readbs nums 0, readbs nums 1, BL.unpack name):h) rest
                           else (reverse h, l)
                       go _ _ = error "Incomplete header" in
                     go []

    readbs :: Read a => [BL.ByteString] -> Int -> a
    readbs l i = read . BL.unpack $ l !! i

    cleanLines = map (BL.filter ('\r' /=)) . BL.lines
