module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = concat
    [ "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
    , "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
    , "824824821-824824827,2121212118-2121212124"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day02.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 11

      it "parses real input (length)" $
        length input `shouldBe` 38

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 1227775554

      it "computes accepted result" $
        part1 input `shouldBe` 30608905813

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 4174379265

      it "computes accepted result" $
        part2 input `shouldBe` 31898925685
