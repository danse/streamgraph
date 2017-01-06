{-# LANGUAGE OverloadedStrings #-}
module Streamgraph where

import Vishnje
import Paths_streamgraph (getDataFileName)
import Data.Time.Clock
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Scientific (fromFloatDigits)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazy

-- make an universal time value using year, month, day
makeUni :: Integer -> Int -> Int -> UTCTime
makeUni y m d = UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

-- | Transform input data to JSON
-- > transform [("a", 0.5, makeUni 2017 1 3), ("b", 0.2, makeUni 2017 1 4)]
-- "[{\"key\":\"a\", \"value\": 0.5, \"date\": \"2017-01-04\"}]"
transform :: [(T.Text, Float, UTCTime)] -> T.Text
transform =
  let myFormat = T.pack . formatTime defaultTimeLocale "%D"
      single (a, b, c) = A.Object (H.fromList [("key", A.String a), ("value", (A.Number . fromFloatDigits) b), ("date", (A.String . myFormat) c)])
  in TextLazy.toStrict . TextLazy.decodeUtf8 . A.encode . A.toJSON . A.Array . V.fromList . map single

options = Options Version2 ChartDiv

streamgraph = customVishnjeFiles options getDataFileName transform
