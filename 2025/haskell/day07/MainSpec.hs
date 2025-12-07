module MainSpec (main) where

import Test.Hspec
import Main hiding (main)
import Control.Arrow
import qualified Data.IntSet as IS

exampleText
  = unlines
    [ ".......S......."
    , "..............."
    , ".......^......."
    , "..............."
    , "......^.^......"
    , "..............."
    , ".....^.^.^....."
    , "..............."
    , "....^.^...^...."
    , "..............."
    , "...^.^...^.^..."
    , "..............."
    , "..^...^.....^.."
    , "..............."
    , ".^.^.^.^.^...^."
    , "..............."
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day07.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        second length exampleInput `shouldBe` (IS.singleton 7, 16)

      it "parses real input (length)" $
        second length input `shouldBe` (IS.singleton 70, 142)

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 21

      it "computes accepted result" $
        part1 input `shouldBe` 1518

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --     part2 input `shouldBe` 0
