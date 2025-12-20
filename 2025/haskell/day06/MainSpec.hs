module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "123 328  51 64 "
    , " 45 64  387 23 "
    , "  6 98  215 314"
    , "*   +   *   +  "
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day06.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 4

      it "parses real input (length)" $
        length input `shouldBe` 1000

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 4277556

      it "computes accepted result" $
        part1 input `shouldBe` 4805473544166

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --     part2 input `shouldBe` 0
