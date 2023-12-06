module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "Time:      7  15   30"
    , "Distance:  9  40  200"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day06.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 3

      it "parses real input (length)" $
        length input `shouldBe` 4

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 288

      it "computes accepted result" $
        part1 input `shouldBe` 1155175

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --      part2 input `shouldBe` 0
