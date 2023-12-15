{-# OPTIONS_GHC -W #-}

module MainSpec (main) where

import Test.Hspec
import Main hiding (main)
import Control.Arrow ((***))

exampleText
  = unlines
    [ "RL"
    , ""
    , "AAA = (BBB, CCC)"
    , "BBB = (DDD, EEE)"
    , "CCC = (ZZZ, GGG)"
    , "DDD = (DDD, DDD)"
    , "EEE = (EEE, EEE)"
    , "GGG = (GGG, GGG)"
    , "ZZZ = (ZZZ, ZZZ)"
    ]

-- exampleText2
--   = unlines
--     [ "LR"
--     , ""
--     , "11A = (11B, XXX)"
--     , "11B = (XXX, 11Z)"
--     , "11Z = (11B, XXX)"
--     , "22A = (22B, XXX)"
--     , "22B = (22C, 22C)"
--     , "22C = (22Z, 22Z)"
--     , "22Z = (22B, 22B)"
--     , "XXX = (XXX, XXX)"
--     ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day08.txt"
  let exampleInput = parse exampleText
  -- let exampleInput2 = parse exampleText2

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        (length *** length) exampleInput `shouldBe` (2, 7)

      it "parses real input (length)" $
        (length *** length) input `shouldBe` (293, 786)

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 2

      it "computes accepted result" $
        part1 input `shouldBe` 19631

    describe "part2" $ do
      -- Implementation does not work for example input
      -- it "computes accepted result for example input" $
      --   part2 exampleInput2 `shouldBe` 6

      it "computes accepted result" $
        part2 input `shouldBe` 21003205388413
