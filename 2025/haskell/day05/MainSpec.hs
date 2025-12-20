module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

-- import Data.Tuple.Extra (both)
import Control.Arrow

exampleText
  = unlines
    [ "3-5"
    , "10-14"
    , "16-20"
    , "12-18"
    , ""
    , "1"
    , "5"
    , "8"
    , "11"
    , "17"
    , "32"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day05.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        (length *** length) exampleInput `shouldBe` (4, 6)

      it "parses real input (length)" $
        (length *** length) input `shouldBe` (190, 1000)

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 3

      it "computes accepted result" $
        part1 input `shouldBe` 811

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 14

      it "computes accepted result" $
        part2 input `shouldBe` 338189277144473
