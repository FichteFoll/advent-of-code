module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "#.##..##."
    , "..#.##.#."
    , "##......#"
    , "##......#"
    , "..#.##.#."
    , "..##..##."
    , "#.#.##.#."
    , ""
    , "#...##..#"
    , "#....#..#"
    , "..##..###"
    , "#####.##."
    , "#####.##."
    , "..##..###"
    , "#....#..#"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day13.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 2

      it "parses real input (length)" $
        length input `shouldBe` 100

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 405

      it "computes accepted result" $
        part1 input `shouldBe` 42974

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 400

      it "computes accepted result" $
        part2 input `shouldBe` 27587
