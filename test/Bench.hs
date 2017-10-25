module Main (main) where

import EagleTree

import Control.Parallel.Strategies

import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Csv (encode)

import Criterion
import Criterion.Main

benches :: Session -> [Benchmark]
benches s = map (\(n,f) -> bench n $ f s) [
    ("string column", whnf (column id "PackVolt*100"))
  , ("int column", whnf (intColumn "PackVolt*100"))
  , ("gps data", nf gpsData)
  , ("gps data parallel", nf gpsDataP)
  , ("csv", \l -> whnf encode (rows l))
  ]
  where gpsDataP ld = gpsData ld `using` parList rdeepseq

main :: IO ()
main = do
  ld <- BL.readFile "test/sample.FDR"
  let stuff = last (parseLog ld)
  defaultMain $ benches stuff
