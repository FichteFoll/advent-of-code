module MainSpec (main) where

import qualified Data.Map.Strict as M
import Test.Hspec
import Main hiding (main)

exampleText
  = unlines
    [ "aaa: you hhh"
    , "you: bbb ccc"
    , "bbb: ddd eee"
    , "ccc: ddd eee fff"
    , "ddd: ggg"
    , "eee: out"
    , "fff: out"
    , "ggg: out"
    , "hhh: ccc fff iii"
    , "iii: out"
    ]

exampleText2
  = unlines
    [ "svr: aaa bbb"
    , "aaa: fft"
    , "fft: ccc"
    , "bbb: tty"
    , "tty: ccc"
    , "ccc: ddd eee"
    , "ddd: hub"
    , "hub: fff"
    , "eee: dac"
    , "dac: fff"
    , "fff: ggg hhh"
    , "ggg: out"
    , "hhh: out"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day11.txt"
  let exampleInput = parse exampleText
  let exampleInput2 = parse exampleText2

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        M.size exampleInput `shouldBe` 10

      it "parses real input (length)" $
        length input `shouldBe` 592

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 5

      it "computes accepted result" $
        part1 input `shouldBe` 782

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput2 `shouldBe` 2

      it "computes accepted result" $
        part2 input `shouldBe` 401398751986160
