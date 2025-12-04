module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "987654321111111"
    , "811111111111119"
    , "234234234234278"
    , "818181911112111"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day03.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 4

      it "parses real input (length)" $
        length input `shouldBe` 200

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 357

      it "computes accepted result" $
        part1 input `shouldBe` 17346

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 3121910778619

      it "computes accepted result" $
        part2 input `shouldBe` 172981362045136
