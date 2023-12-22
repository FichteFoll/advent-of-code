module MainSpec (main) where

import Test.Hspec
import Main hiding (main)
import Data.Graph (vertices)
import Distribution.Utils.Generic (fstOf3)

exampleText1
  = unlines
    [ "broadcaster -> a, b, c"
    , "%a -> b"
    , "%b -> c"
    , "%c -> inv"
    , "&inv -> a"
    ]

exampleText2
  = unlines
    [ "broadcaster -> a"
    , "%a -> inv, con"
    , "&inv -> b"
    , "%b -> con"
    , "&con -> output"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day20.txt"
  let exampleInput1 = parse exampleText1
  let exampleInput2 = parse exampleText2

  hspec $ do
    describe "parse" $ do
      it "parses example input 1 (num vertices)" $
        length (vertices $ fstOf3 exampleInput1) `shouldBe` 5

      it "parses example input 2 (num vertices)" $
        length (vertices $ fstOf3 exampleInput2) `shouldBe` 5

      it "parses real input (num vertices)" $
        length (vertices $ fstOf3 input) `shouldBe` 58

    describe "part1" $ do
      it "computes expected result after 1 button push for example input 1" $
        pulseProduct (push 1 exampleInput1) `shouldBe` 32

      it "computes accepted result for example input 1" $
        part1 exampleInput1 `shouldBe` 32000000

      it "computes accepted result for example input 2" $
        part1 exampleInput2 `shouldBe` 11687500

      it "computes accepted result" $
        part1 input `shouldBe` 730797576

    -- describe "part2" $ do
    --   it "computes accepted result for example input" $
    --     part2 exampleInput `shouldBe` 0

    --   it "computes accepted result" $
    --     part2 input `shouldBe` 0
