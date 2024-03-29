module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ ".|...\\...."
    , "|.-.\\....."
    , ".....|-..."
    , "........|."
    , ".........."
    , ".........\\"
    , "..../.\\\\.."
    , ".-.-/..|.."
    , ".|....-|.\\"
    , "..//.|...."
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day16.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` (10^2)

      it "parses real input (length)" $
        length input `shouldBe` (110^2)

    describe "part1" $ do
      it "visits all fields for a simple 2x2 input" $
        part1 (parse "\\\\\n\\/") `shouldBe` 4

      it "visits 5 fields for a simple 3x2 input" $
        part1 (parse "\\.|\n\\.|") `shouldBe` 5

      it "visits 7 fields for a simple 3x3 input" $
        part1 (parse "\\|.\n..\\\n-./") `shouldBe` 7

      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 46

      it "computes accepted result" $
        part1 input `shouldBe` 7951

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 51

      -- needs 1 minute to compute with optimizations
      -- it "computes accepted result" $
      --   part2 input `shouldBe` 8148
