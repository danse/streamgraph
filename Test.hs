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

aggregate = toTimeSeries 1 . map toPoint

modThree :: Int -> Int
modThree = flip mod 3

main :: IO ()
main = hspec $ do
  describe "aggregate" $ do
    it "first case" $ do
      (length . aggregate) toAggregate `shouldBe` (length keys * length dates)
  describe "the monoid" $ do
    it "appends as expected" $ do
      (getText (mappend (TextFloat "a" 1) (TextFloat "a" 2))) `shouldBe` "a"
  describe "groupWith" $ do
    it "groups even numbers" $ do
      (groupWith modThree [1, 2, 4, 3, 6]) `shouldBe` [[3,6], [1,4], [2]]
      
