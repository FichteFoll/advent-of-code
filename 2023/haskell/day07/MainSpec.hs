module MainSpec (main) where

import Test.Hspec
import Main hiding (main)
import Data.List (sortBy)

exampleText
  = unlines
    [ "32T3K 765"
    , "T55J5 684"
    , "KK677 28"
    , "KTJJT 220"
    , "QQQJA 483"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day07.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 5

      it "parses real input (length)" $
        length input `shouldBe` 1000

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 6440

      it "computes accepted result" $
        part1 input `shouldBe` 250453939

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --      part2 input `shouldBe` 0

    describe "cardKey" $ do
      let testCardKey cardA cmp cardB = it (concat ["compares ", show cardA, " ", show cmp, " ", show cardB]) $ do
          cardKey (cardA, 0) (cardB, 0) `shouldBe` cmp

      testCardKey "34567" GT "23456"
      testCardKey "33332" GT "2AAAA"
      testCardKey "77888" GT "77788"
      testCardKey "QQQJA" GT "KTJJT"

      testCardKey "32T3K" LT "KTJJT"
      testCardKey "KTJJT" LT "KK677"
      testCardKey "KK677" LT "T55J5"
      testCardKey "T55J5" LT "QQQJA"


      it "sorts example input via sortBy" $
        (map fst . sortBy cardKey $ exampleInput) `shouldBe`
          [ "32T3K"
          , "KTJJT"
          , "KK677"
          , "T55J5"
          , "QQQJA"
          ]
