{-# LANGUAGE OverloadedStrings #-}
module Streamgraph where

import Visie
import Visie.Data (TextFloat(TextFloat, getText), toText)
import Visie.ToTimeSeries
import Visie.Index
import Paths_streamgraph (getDataFileName)
import Data.Time.Clock
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Scientific (fromFloatDigits)
import Data.Hashable
import Data.List (sortOn)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

type Point = Timestamped TextFloat

dateFormat :: UTCTime -> T.Text
dateFormat = T.pack . formatTime defaultTimeLocale "%D"

-- make an universal time value using year, month, day
makeUni :: Integer -> Int -> Int -> UTCTime
makeUni y m d = UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

options = defaultOptions { d3Version = Version2, indexType = ChartDiv }

toPoint :: (T.Text, Float, UTCTime) -> Point
toPoint (te, fl, ti) = Timestamped (TextFloat te fl) ti

-- if some dates will be missing for some labels, a runtime error will
-- be thrown. In order to prevent that, this function inefficiently
-- generates empty data points for all labels and dates
addAllPoints :: [Point] -> [Point]
addAllPoints existing = existing ++ allPoints
  where allPoints = [Timestamped (TextFloat te 0) ti | te <- texts, ti <- times]
        texts = map (getText . getStamped) existing
        times = map getTime existing

-- | given a function `f` and a list `l`, produce a list of lists of
-- elements of `l` that produce the same result when `f` is applied to
-- them
groupWith :: (Eq b, Hashable b) => (a -> b) -> [a] -> [[a]]
groupWith f = H.elems . foldr myInsert H.empty
  where myInsert a = H.insertWith (++) (f a) [a]

-- | This function groups points with the same description, which are
-- then converted to time series and concatenated in a flat list of
-- points. So points with the same description are considered like a
-- single time series, and are one after the other in the final
-- list. This might affect the proper functioning of the drawing
-- logic, so test after changes
toTimeSeries :: Int -> [Point] -> [Point]
toTimeSeries days = concat . map (convert seconds) . toStreams
  where toStreams = groupWith (getText . getStamped)
        seconds = fromIntegral (days * 60 * 60 * 24)

transform days = toText . toTimeSeries days . addAllPoints . map toPoint

streamgraph days = customVisie options getDataFileName (transform days)
