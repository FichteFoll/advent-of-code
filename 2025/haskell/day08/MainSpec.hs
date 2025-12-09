module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "162,817,812"
    , "57,618,57"
    , "906,360,560"
    , "592,479,940"
    , "352,342,300"
    , "466,668,158"
    , "542,29,236"
    , "431,825,988"
    , "739,650,466"
    , "52,470,668"
    , "216,146,977"
    , "819,987,18"
    , "117,168,530"
    , "805,96,715"
    , "346,949,466"
    , "970,615,88"
    , "941,993,340"
    , "862,61,35"
    , "984,92,344"
    , "425,690,689"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day08.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 20

      it "parses real input (length)" $
        length input `shouldBe` 1000

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1' 10 exampleInput `shouldBe` 40

      it "computes accepted result" $
        part1' 1000 input `shouldBe` 102816

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 25272

      it "computes accepted result" $
        part2 input `shouldBe` 100011612
