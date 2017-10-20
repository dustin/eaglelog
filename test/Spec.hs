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

import Debug.Trace

import EagleTree

test_data = BL.readFile "test/sample.FDR"

withTestData :: ([Session] -> IO ()) -> IO ()
withTestData f = do
  d <- test_data
  f (parseLog d)

reddit l = assertEqual "" [4436, 291] $ trace (show l) $ map (length.colVals) l

tests = [
  testCase "read the data" $ withTestData reddit
  ]

main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests
            opts { ropt_hide_successes = Just True }
