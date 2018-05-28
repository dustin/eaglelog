{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import EagleTree

import Control.Monad (forM_)
import Data.Csv (encodeByName, encodeByNameWith, defaultEncodeOptions, EncodeOptions(..))
import System.Environment
import System.IO
import Text.RawString.QQ
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

writeCSV :: Int -> Session -> IO ()
writeCSV i sess = withFile ("session_" ++ show i ++ ".csv") WriteMode wr
  where wr h = (BL.hPut h) . encodeByName names $ rows sess
        names = V.fromList . map BC.pack $ colNames sess

kmlTop :: String
kmlTop = [r|<kml xmlns="http://earth.google.com/kml/2.0">
<Document>
<Style id="1">
<LineStyle>
<color>ff0000ff</color>
<width>5</width>
</LineStyle>
</Style>
<Style id="2">
<LineStyle>
<color>7f0000ff</color>
<width>4</width>
</LineStyle>
</Style>
<Placemark>
<styleUrl>#1</styleUrl>
<LineString>
<altitudeMode>absolute</altitudeMode>
<coordinates>
|]

kmlBottom :: String
kmlBottom = [r|
</coordinates>
</LineString>
</Placemark>
</Document>
</kml>
|]

writeKML :: Int -> Session -> IO ()
writeKML i sess = withFile ("session_" ++ show i ++ ".kml") WriteMode wr
  where wr h = do
          hPutStr h kmlTop
          (BL.hPut h) . encodeByNameWith defaultEncodeOptions {
            encUseCrLf=False,
            encIncludeHeader=False
            } (V.fromList ["GPSLon", "GPSLat", "GPSAlt"]) $ rows sess
          hPutStr h kmlBottom

main :: IO ()
main = do
  args <- getArgs
  let (file:_fields) = args
  etdata <- BL.readFile file
  forM_ (zip [1..] (parseLog etdata)) $ uncurry writeCSV
  forM_ (zip [1..] (parseLog etdata)) $ uncurry writeKML
