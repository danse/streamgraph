{-# LANGUAGE OverloadedStrings #-}
module Streamgraph where

import Vishnje
import Paths_streamgraph (getDataFileName)
import Data.Time.Clock
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Scientific (fromFloatDigits)
import Data.Hashable
import Data.List (sortOn)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazy

-- make an universal time value using year, month, day
makeUni :: Integer -> Int -> Int -> UTCTime
makeUni y m d = UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

type GraphInput = [(T.Text, Float, UTCTime)]

dateFormat = T.pack . formatTime defaultTimeLocale "%D"

lastOfTriplet (_, _, x) = x

-- given a list of pairs, generate all possible pairs
allPairs p = [(f, s) | f <- allFirsts, s <- allSeconds]
  where allFirsts = map fst p
        allSeconds = map snd p

-- every key has to be present every day, otherwise there will be a
-- runtime error in a D3 function. This function fills the map with
-- zeroes on the keys that are missing
fillWithZeroes m =
  let setDefault k = H.insertWith (+) k 0
      allKeys = (allPairs . H.keys) m
        in foldr setDefault m allKeys

-- | Transform input data to JSON
-- > transform [("a", 0.5, makeUni 2017 1 3), ("b", 0.2, makeUni 2017 1 4)]
-- "[{\"key\":\"a\", \"value\": 0.5, \"date\": \"2017-01-04\"}]"
transform :: GraphInput -> T.Text
transform =
  let single (a, b, c) = A.Object (H.fromList [("key", A.String a), ("value", (A.Number . fromFloatDigits) b), ("date", (A.String . dateFormat) c)])
  in TextLazy.toStrict . TextLazy.decodeUtf8 . A.encode . A.toJSON . A.Array . V.fromList . map single . sortOn lastOfTriplet

options = Options Version2 ChartDiv

streamgraph = customVishnjeFiles options getDataFileName transform

instance Hashable UTCTime where
  hashWithSalt = hashUsing dateFormat

atMidnight :: UTCTime -> UTCTime
atMidnight (UTCTime day dayTime) = UTCTime day (secondsToDiffTime 0)

aggregate :: GraphInput -> GraphInput
aggregate = map fromMapStructure . H.toList . fillWithZeroes . foldr myInsert H.empty
  where fromMapStructure ((x, z), y) = (x, y, z)
        myInsert (x, y, z) = H.insertWith (+) (x, atMidnight z) y

streamgraphAggregate = streamgraph . aggregate
