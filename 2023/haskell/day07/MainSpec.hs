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

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 5905

      it "computes accepted result" $
         part2 input `shouldBe` 0
         -- < 248859687

    describe "cardKey" $ do
      let testCardKey j cardA cmp cardB = it (concat ["compares ", show cardA, " ", show cmp, " ", show cardB, ", jokers: ", show j]) $ do
          cardKey j (cardA, 0) (cardB, 0) `shouldBe` cmp

      testCardKey False "55555" EQ "55555"
      testCardKey False "34567" GT "23456"
      testCardKey False "33332" GT "2AAAA"
      testCardKey False "77888" GT "77788"
      testCardKey False "QQQJA" GT "KTJJT"

      testCardKey False "32T3K" LT "KTJJT"
      testCardKey False "KTJJT" LT "KK677"
      testCardKey False "KK677" LT "T55J5"
      testCardKey False "T55J5" LT "QQQJA"

      testCardKey True "J8882" LT "TTTT2"
      testCardKey True "2345J" LT "2344J"
      testCardKey True "2223J" GT "JJJ32"
      testCardKey True "222JJ" GT "JJ222"

      it "sorts example input via sortBy (no jokers)" $
        (map fst . sortBy (cardKey False) $ exampleInput) `shouldBe`
          [ "32T3K"
          , "KTJJT"
          , "KK677"
          , "T55J5"
          , "QQQJA"
          ]

      it "sorts example input via sortBy (jokers)" $
        (map fst . sortBy (cardKey True) $ exampleInput) `shouldBe`
          [ "32T3K"
          , "KK677"
          , "T55J5"
          , "QQQJA"
          , "KTJJT"
          ]
