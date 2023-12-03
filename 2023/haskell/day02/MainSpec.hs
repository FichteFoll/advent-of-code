{-# LANGUAGE OverloadedStrings #-}

module MainSpec (main) where

import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]


main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day02.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        length exampleInput `shouldBe` 5
      it "parses example input (head)" $
        head exampleInput `shouldBe`
          [ [("blue", 3), ("red", 4)]
          , [("red", 1), ("green", 2), ("blue", 6)]
          , [("green", 2)]
          ]

      it "parses real input (length)" $
        length input `shouldBe` 100

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 8

      it "computes accepted result" $
        part1 input `shouldBe` 2377

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 2286

      it "computes accepted result" $
         part2 input `shouldBe` 71220
