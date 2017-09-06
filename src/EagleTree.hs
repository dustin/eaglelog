module EagleTree
    ( parse_log
    , Log, Session
    ) where

import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isSpace)

rstrip = reverse . dropWhile isSpace . reverse

type Log = [Session]

data SessionHeader = SessionHeader (Int, Int, String) deriving (Show)

data Session = Session { colNames :: [String]
                       , colVals :: [String]
                       }

clean_lines l = map (BL.filter ('\r' /=)) $ BL.lines l

readbs :: Read a => [BL.ByteString] -> Int -> a
readbs l i = read $ BL.unpack $ l !! i


parse_headers :: [BL.ByteString] -> ([SessionHeader], [BL.ByteString])
parse_headers = let go h l@(a:b:rest) =
                      let nums = BL.words b
                          name = a in
                        if length nums == 2 then
                          go (SessionHeader(readbs nums 0, readbs nums 1, BL.unpack name):h) rest
                        else (h, l) in
                  go []

parse_log :: BL.ByteString -> Int
parse_log f = let l = dropWhile (\l -> BL.unpack l /= "All Sessions") $ clean_lines f
                  (shdr, rest) = parse_headers (drop 2 l)
                  fields = map BL.unpack $ BL.words $ head rest
                  readings = tail rest in
                (trace $ show fields) $ length readings
