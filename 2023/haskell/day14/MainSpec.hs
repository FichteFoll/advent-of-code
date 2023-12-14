module MainSpec (main) where

import Test.Hspec
import Main hiding (main)
import Control.Arrow (first)

exampleText
  = unlines
    [ "O....#...."
    , "O.OO#....#"
    , ".....##..."
    , "OO.#O....O"
    , ".O.....O#."
    , "O.#..O.#.#"
    , "..O..#O..O"
    , ".......O.."
    , "#....###.."
    , "#OO..#...."
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day14.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        first length exampleInput `shouldBe` (18 + 17, (10, 10))

      it "parses real input (length)" $
        first length input `shouldBe` (2029 + 1626, (100, 100))

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 136

      it "computes accepted result" $
        part1 input `shouldBe` 109939

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 64

      -- runs for about 3s
      it "computes accepted result" $
        part2 input `shouldBe` 101010
