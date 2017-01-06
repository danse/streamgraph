{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Streamgraph

dates = [makeUni 2017 1 6,
         makeUni 2017 1 7,
         makeUni 2017 1 8,
         makeUni 2017 1 9,
         makeUni 2017 1 10,
         makeUni 2017 1 11]

keys = ["a", "b", "c"]

toAggregate = [(k, v, d) | k <- keys, v <- [1..10], d <- dates]

main :: IO ()
main = hspec $ do
  describe "aggregate" $ do
    it "first case" $ do
      (length . aggregate) toAggregate `shouldBe` (length keys * length dates)
