import System.Environment (getArgs)
import Test.Framework (defaultMainWithOpts, interpretArgsOrExit, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Runners.Options
import Test.HUnit
import Test.HUnit.Approx
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL

import EagleTree

test_data = BL.readFile "test/sample.FDR"

testStringColumn l = assertEqual "" (Right (replicate 3 "1486")) $ take 3 <$> column id "PackVolt*100" (last l)

testIntColumn l = assertEqual "" (Right (replicate 3 1486)) $ take 3 <$> intColumn "PackVolt*100" (last l)

testFloatColumn l = assertEqual "" (Right (replicate 3 0.0)) $ take 3 <$> floatColumn "GPSDist" (last l)

testGPSData l = assertEqual "" (replicate 3 ETGPSData {gpsLat = 36.988275
                                                      , gpsLon = -122.065895
                                                      , gpsAlt = 166
                                                      , gpsSpeed = 0
                                                      , gpsCourse = 273.2
                                                      , gpsDist = 0
                                                      , gpsNumSats = 15} ) $ take 3 $ gpsData (last l)

testColNames l = assertEqual "" ["Aileron_In","Aileron_Out","Airspeed"] $ take 3 $ colNames (last l)

tests = [
  testCase "string column" $ td testStringColumn,
  testCase "int column" $ td testIntColumn,
  testCase "float column" $ td testFloatColumn,
  testCase "column names" $ td testColNames,
  testCase "GPS data" $ td testGPSData
  ]
  where td f = test_data >>= \d -> f (parseLog d)

main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests
            opts { ropt_hide_successes = Just True }
