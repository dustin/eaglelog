import System.Environment (getArgs)
import Test.Framework (Test, defaultMainWithOpts, interpretArgsOrExit)
import Test.Framework.Providers.HUnit
import Test.Framework.Runners.Options
import Test.HUnit (Assertion, assertEqual)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Csv (encode, encodeByName)

import EagleTree

testStringColumn :: [Session] -> Assertion
testStringColumn l = assertEqual "" (Right (replicate 3 "1486")) $ take 3 <$> column id "PackVolt*100" (last l)

testIntColumn :: [Session] -> Assertion
testIntColumn l = assertEqual "" (Right (replicate 3 1486)) $ take 3 <$> intColumn "PackVolt*100" (last l)

testFloatColumn :: [Session] -> Assertion
testFloatColumn l = assertEqual "" (Right (replicate 3 0.0)) $ take 3 <$> floatColumn "GPSDist" (last l)

testGPSData :: [Session] -> Assertion
testGPSData l = assertEqual "" (replicate 3 ETGPSData {gpsLat = 36.988275
                                                      , gpsLon = -122.065895
                                                      , gpsAlt = 166
                                                      , gpsSpeed = 0
                                                      , gpsCourse = 273.2
                                                      , gpsDist = 0
                                                      , gpsNumSats = 15} ) $ take 3 $ gpsData (last l)

testColNames :: [Session] -> Assertion
testColNames l = assertEqual "" ["Milliseconds", "IsEvent", "EventError"] $ take 3 $ colNames (last l)

testRows :: [Session] -> Assertion
testRows l = assertEqual "" 291 $ length.rows $ last l

testCSVRecord :: [Session] -> Assertion
testCSVRecord l = assertEqual "" x $ BL.unpack.encode.take 1.rows $ last l
  where x = "443700,1,50,3,50,49,50,254,50,50,0,15,1486,30,0,6,98,37,20,520,94,4857,500,1189,0,36.988275000000,-122.065895000000,166.0,0.0,273.2,0.0,77707100.0000,15,7,0,0,0,0,0,0,314,-8,6,-46,50,49,50,254,254,49,50,156,\"06/21/2017,13:35:07.10\",7\r\n"

testCSVNamedRecord :: [Session] -> Assertion
testCSVNamedRecord l = assertEqual "" x $ BL.unpack.encodeByName (v ["Milliseconds", "IsEvent", "EventError"]).take 1.rows $ last l
  where v = V.fromList . map BC.pack
        x = "Milliseconds,IsEvent,EventError\r\n443700,1,50\r\n"

tests :: [Test]
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
  where td f = BL.readFile "test/sample.FDR" >>= \d -> f (parseLog d)

main :: IO ()
main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests
            opts { ropt_hide_successes = Just True }
