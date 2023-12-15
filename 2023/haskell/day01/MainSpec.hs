module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleInput1
  = [ "1abc2"
    , "pqr3stu8vwx"
    , "a1b2c3d4e5f"
    , "treb7uchet"
    ]

exampleInput2
  = [ "two1nine"
    , "eightwothree"
    , "abcone2threexyz"
    , "xtwone3four"
    , "4nineeightseven2"
    , "zoneight234"
    , "7pqrstsixteen"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day01.txt"

  hspec $ do
    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput1 `shouldBe` 142

      it "computes accepted result" $
        part1 input `shouldBe` 54951

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput2 `shouldBe` 281
      it "computes accepted result" $
        part2 input `shouldBe` 55218

    describe "extractDigits2" $ do
      it "extracts 'two'" $
        extractDigits2 "two" `shouldBe` [2]
      it "extracts 'two1'" $
        extractDigits2 "two1" `shouldBe` [2, 1]
      it "extracts 'xxxxtwoxxxx1xxxx'" $
        extractDigits2 "xxxxtwoxxxx1xxxx" `shouldBe` [2, 1]
      it "extracts all digit words" $
        extractDigits2 "one two three four five six seven eight nine" `shouldBe` [1..9]
      it "extracts overlapping 'eighthreesevenine'" $
        extractDigits2 "eighthreesevenine" `shouldBe` [8, 3, 7, 9]

    describe "calcLine" $ do
      it "computes [1, 2]" $
        calcLine [1, 2] `shouldBe` 12
      it "computes [2, 1]" $
        calcLine [2, 1] `shouldBe` 21
      it "computes [1]" $
        calcLine [1] `shouldBe` 11
      it "computes [0, 9]" $
        calcLine [0, 5, 9] `shouldBe` 9
