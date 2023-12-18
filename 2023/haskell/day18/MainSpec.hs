module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "R 6 (#70c710)"
    , "D 5 (#0dc571)"
    , "L 2 (#5713f0)"
    , "D 2 (#d2c081)"
    , "R 2 (#59c680)"
    , "D 2 (#411b91)"
    , "L 5 (#8ceee2)"
    , "U 2 (#caa173)"
    , "L 1 (#1b58a2)"
    , "U 2 (#caa171)"
    , "R 2 (#7807d2)"
    , "U 3 (#a77fa3)"
    , "L 2 (#015232)"
    , "U 2 (#7a21e3)"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day18.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 14

      it "parses real input (length)" $
        length input `shouldBe` 784

    -- describe "part1" $ do
    --   it "computes accepted result for example input" $
    --     part1 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --     part1 input `shouldBe` 0

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --     part2 input `shouldBe` 0
