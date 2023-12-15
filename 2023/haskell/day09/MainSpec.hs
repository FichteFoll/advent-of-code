module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "0 3 6 9 12 15"
    , "1 3 6 10 15 21"
    , "10 13 16 21 30 45"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day09.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 3

      it "parses real input (length)" $
        length input `shouldBe` 200

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 114

      it "computes accepted result" $
        part1 input `shouldBe` 1842168671

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 2

      it "computes accepted result" $
        part2 input `shouldBe` 903
