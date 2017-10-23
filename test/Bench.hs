module Main (main) where

import EagleTree

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Csv (encode)

import Criterion
import Criterion.Main


benchStringColumn logData = bench "string column" $ whnf (column id "PackVolt*100") logData
benchIntColumn logData    = bench "int column" $ whnf (intColumn "PackVolt*100") logData
benchGPSData logData      = bench "gps data" $ whnf gpsData logData
benchCSV logData          = bench "csv" $ whnf encode (rows logData)

main = do
  ld <- BL.readFile "test/sample.FDR"
  let stuff = last (parseLog ld)
  defaultMain $ map ($ stuff) [benchStringColumn, benchIntColumn, benchGPSData, benchCSV]
