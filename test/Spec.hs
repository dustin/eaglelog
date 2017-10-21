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
import qualified Data.Map.Strict as Map

import EagleTree

test_data = BL.readFile "test/sample.FDR"

stringColumn l = assertEqual "" (replicate 3 "1486") $ take 3 $ column id "PackVolt*100" (last l)

intColumn l = assertEqual "" (replicate 3 1486) $ take 3 $ column read "PackVolt*100" (last l)

floatColumn l = assertEqual "" (replicate 3 "0.0") $ take 3 $ column id "GPSDist" (last l)

reddit l = assertEqual "" [4436, 291] $ map (length.colVals) l

testColNames l = assertEqual "" ["Aileron_In","Aileron_Out","Airspeed"] $ take 3 $ (Map.keys.colNames) (last l)

tests = [
  testCase "read the data" $ td reddit,
  testCase "string column" $ td stringColumn,
  testCase "int column" $ td intColumn,
  testCase "float column" $ td floatColumn,
  testCase "column names" $ td testColNames
  ]
  where td f = test_data >>= \d -> f (parseLog d)

main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests
            opts { ropt_hide_successes = Just True }
