{-# LANGUAGE OverloadedStrings #-}
import Streamgraph
import Data.Monoid ((<>))

d = [("a", 10, makeUni 2017 1 1),
     ("b", 5, makeUni 2017 1 1),
     ("c", 30, makeUni 2017 1 1),
     ("a", 10, makeUni 2017 1 2),
     ("b", 10, makeUni 2017 1 2),
     ("c", 15, makeUni 2017 1 2),
     ("a", 10, makeUni 2017 1 3),
     ("b", 15, makeUni 2017 1 3),
     ("c", 20, makeUni 2017 1 3)]

dates = [makeUni 2017 1 6,
         makeUni 2017 1 7,
         makeUni 2017 1 8,
         makeUni 2017 1 9,
         makeUni 2017 1 10,
         makeUni 2017 1 11]

e1 = [(k, v, d) | k <- ["a", "b", "c"], v <- [1..10], d <- dates]
e2 = [("a", 100, makeUni 2017 1 6),
      ("b", 100, makeUni 2017 1 8),
      ("c", 100, makeUni 2017 1 11)]

main = do
  streamgraph d
  streamgraphAggregate (e1 <> e2)
