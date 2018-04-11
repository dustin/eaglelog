import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Test.QuickCheck.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit

import Data.Csv (encode, encodeByName)

import EagleTree

columnTest :: (Show t, Eq t) => [Session] -> (String -> Session -> Either String [t]) -> String -> [t] -> Assertion
columnTest l f k want = assertEqual "" (Right want) $ take 3 <$> f k (last l)

testIntColumn :: [Session] -> Assertion
testIntColumn l = columnTest l intColumn "PackVolt*100" (replicate 3 1486)

testFloatColumn :: [Session] -> Assertion
testFloatColumn l = columnTest l floatColumn "GPSDist" (replicate 3 0.0)

testStringColumn :: [Session] -> Assertion
testStringColumn l = columnTest l (column id) "PackVolt*100" (replicate 3 "1486")

testGPSData :: [Session] -> Assertion
testGPSData l = assertEqual "" (replicate 3 ETGPSData {gpsLat = 36.988275
                                                      , gpsLon = -122.065895
                                                      , gpsAlt = 166
                                                      , gpsSpeed = 0
                                                      , gpsCourse = 273.2
                                                      , gpsDist = 0
                                                      , gpsNumSats = 15} ) $ (take 3 . gpsData . last) l

testColNames :: [Session] -> Assertion
testColNames l = assertEqual "" ["Milliseconds", "IsEvent", "EventError"] $ (take 3 . colNames . last) l

testRows :: [Session] -> Assertion
testRows l = assertEqual "" 291 $ (length.rows.last) l

testCSVRecord :: [Session] -> Assertion
testCSVRecord l = assertEqual "" x $ (BL.unpack.encode.take 1.rows.last) l
  where x = "443700,1,50,3,50,49,50,254,50,50,0,15,1486,30,0,6,98,37,20,520,94,4857,500,1189,0,36.988275000000,-122.065895000000,166.0,0.0,273.2,0.0,77707100.0000,15,7,0,0,0,0,0,0,314,-8,6,-46,50,49,50,254,254,49,50,156,\"06/21/2017,13:35:07.10\",7\r\n"

testCSVNamedRecord :: [Session] -> Assertion
testCSVNamedRecord l = assertEqual "" x $ (BL.unpack.encodeByName (v ["Milliseconds", "IsEvent", "EventError"]).take 1.rows.last) l
  where v = V.fromList . map BC.pack
        x = "Milliseconds,IsEvent,EventError\r\n443700,1,50\r\n"

tests :: [TestTree]
tests = map (\(name, fun) -> testCase name (td fun)) [
  ("string column", testStringColumn),
  ("int column", testIntColumn),
  ("float column", testFloatColumn),
  ("column names", testColNames),
  ("GPS data", testGPSData),
  ("get rows", testRows),
  ("csv record", testCSVRecord),
  ("csv named record", testCSVNamedRecord)
  ]
  where td f = BL.readFile "test/sample.FDR" >>= f . parseLog

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
