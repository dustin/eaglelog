module Main where

import EagleTree

import Control.Monad (forM_)
import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as BL

writeResults :: Int -> Session -> IO ()
writeResults id sess = withFile ("session_" ++ show id ++ ".csv") WriteMode wr
  where wr h = pure ()

main :: IO ()
main = do
  args <- getArgs
  let (file:fields) = args
  etdata <- BL.readFile file
  forM_ (zip [1..] (parseLog etdata)) $ uncurry writeResults
