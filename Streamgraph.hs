{-# LANGUAGE OverloadedStrings #-}
module Streamgraph where

import Visie
import Visie.Data (TextFloat(TextFloat, getText), toText)
import Visie.ToTimeSeries
import Visie.Index
import Visie.Data
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
-- then mapped and concatenated in a flat list of points. So points
-- with the same description are considered like a single time
-- series. Order might affect the proper functioning of the drawing
-- logic, so test after changes
toTimeSeries :: ([Point] -> [Point]) -> [Point] -> [Point]
toTimeSeries onEachStream = concat
                            . map onEachStream
                            . toStreams
  where toStreams :: [Point] -> [[Point]]
        toStreams = groupWith (getText . getStamped)

-- | sort `elements` concatenating their monoid when their time falls
-- within the same interval
convertPoints :: NominalDiffTime -> [Point] -> [Point]
convertPoints interval elements = sampler sorted
  where sorted = sortOn getTime elements
        times = iterator interval (getTime (head sorted))
        sampler = consume times
        iterator :: NominalDiffTime -> UTCTime -> [UTCTime]
        iterator interval start = iterate (addUTCTime interval) start
        getText (Timestamped (TextFloat t _) _) = t
        label = getText $ head elements
        -- at every call, the recursive function returns a processed list, and
        -- it gets a non processed list and a reference date about the last
        -- emitted element. if the next element would have a date greater than
        -- the reference + the interval, a filling element is
        -- created. otherwise, the function will look ahead and merge all
        -- elements within the same interval, pick a representative date for
        -- the merged elements and use it as the new reference
        consume :: [UTCTime] -> [Point] -> [Point]
        consume (t:ts) [] = []
        consume (t:ts) elements
          | length preceding == 0 = filled : rest
          | otherwise = foldl (merge t) filled preceding : rest
          where (preceding, succeeding) = span ((<= t) . getTime) elements
                filled = toPoint (label, 0, t)
                rest = consume ts succeeding
                merge :: Monoid a => UTCTime -> Timestamped a -> Timestamped a -> Timestamped a
                merge t (Timestamped a _) (Timestamped b _) = Timestamped (mappend a b) t

streamgraph :: Int -> Maybe Int -> [(T.Text, Float, UTCTime)] -> IO ()
streamgraph days maybeLastPoints =
  customVisie options getDataFileName transform
  where transform = toText
                    . toTimeSeries onEachStream
                    . addAllPoints
                    . map toPoint
        -- | turn each stream into a time series and take the last n
        -- points
        onEachStream :: [Point] -> [Point]
        onEachStream = maybeLastN maybeLastPoints . convertPoints seconds
        seconds = fromIntegral (days * 60 * 60 * 24)
        maybeLastN :: Maybe Int -> [a] -> [a]
        maybeLastN Nothing  l = l
        maybeLastN (Just n) l = drop (length l - n) l
