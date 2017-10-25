module Main where

import EagleTree

import Control.Monad (forM_)
import System.IO
import System.Environment
import Data.Csv (encodeByName)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

writeResults :: Int -> Session -> IO ()
writeResults i sess = withFile ("session_" ++ show i ++ ".csv") WriteMode wr
  where wr h = (BL.hPut h) . encodeByName names $ rows sess
        names = V.fromList . map BC.pack $ colNames sess

main :: IO ()
main = do
  args <- getArgs
  let (file:_fields) = args
  etdata <- BL.readFile file
  forM_ (zip [1..] (parseLog etdata)) $ uncurry writeResults
