module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "467..114.."
    , "...*......"
    , "..35..633."
    , "......#..."
    , "617*......"
    , ".....+.58."
    , "..592....."
    , "......755."
    , "...$.*...."
    , ".664.598.."
    ]


main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day03.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 10

      it "parses real input (length)" $
        length input `shouldBe` 140

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 4361

      it "computes accepted result" $
        part1 input `shouldBe` 536576

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --      part2 input `shouldBe` 0
