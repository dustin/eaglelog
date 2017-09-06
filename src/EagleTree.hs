module EagleTree
    ( parse_log
    , Session
    ) where

import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)

rstrip = reverse . dropWhile isSpace . reverse

data SessionHeader = SessionHeader (Int, Int, String) deriving (Show)

data Session = Session { name :: String
                       , colNames :: Map.Map String Int
                       , colVals :: [String]
                       }

instance Show Session where
  show (Session n cn cv) =
    n ++ " cols=" ++ (show cn) ++ ", " ++ (show $ length cv) ++ " readings"

clean_lines l = map (BL.filter ('\r' /=)) $ BL.lines l

readbs :: Read a => [BL.ByteString] -> Int -> a
readbs l i = read $ BL.unpack $ l !! i


parse_headers :: [BL.ByteString] -> ([SessionHeader], [BL.ByteString])
parse_headers = let go h l@(a:b:rest) =
                      let nums = BL.words b
                          name = a in
                        if length nums == 2 then
                          go (SessionHeader(readbs nums 0, readbs nums 1, BL.unpack name):h) rest
                        else (reverse h, l) in
                  go []

parse_log :: BL.ByteString -> [Session]
parse_log f = let l = dropWhile (\l -> BL.unpack l /= "All Sessions") $ clean_lines f
                  (shdr, rest) = parse_headers (drop 2 l)
                  fields = Map.fromList $ zip (map BL.unpack $ BL.words $ head rest) [0..]
                  readings = tail rest in
                map (\ (SessionHeader (f, t, n)) -> Session n fields $ (take (t-f)) . drop f $ map BL.unpack rest ) shdr
