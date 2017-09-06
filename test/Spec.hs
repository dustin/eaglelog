import System.Environment (getArgs)
import Test.Framework (defaultMainWithOpts, interpretArgsOrExit, testGroup)
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

reddit = do
  d <- test_data
  let l = parse_log d
  assertEqual "" 4728 l

tests = [
  testCase "read the data" reddit
  ]

main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests
            opts { ropt_hide_successes = Just True }
