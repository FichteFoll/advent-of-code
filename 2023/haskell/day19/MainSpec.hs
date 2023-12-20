module MainSpec (main) where

import Test.Hspec
import Main hiding (main)
import Control.Lens (bimap)

exampleText
  = unlines
    [ "px{a<2006:qkq,m>2090:A,rfg}"
    , "pv{a>1716:R,A}"
    , "lnx{m>1548:A,A}"
    , "rfg{s<537:gd,x>2440:R,A}"
    , "qs{s>3448:A,lnx}"
    , "qkq{x<1416:A,crn}"
    , "crn{x>2662:A,R}"
    , "in{s<1351:px,qqz}"
    , "qqz{s>2770:qs,m<1801:hdj,R}"
    , "gd{a>3333:R,R}"
    , "hdj{m>838:A,pv}"
    , ""
    , "{x=787,m=2655,a=1222,s=2876}"
    , "{x=1679,m=44,a=2067,s=496}"
    , "{x=2036,m=264,a=79,s=2244}"
    , "{x=2461,m=1339,a=466,s=291}"
    , "{x=2127,m=1623,a=2188,s=1013}"
    ]

main :: IO ()
main = do
  input <- parse <$> readFile "../../input/day19.txt"
  let exampleInput = parse exampleText

  hspec $ do
    describe "parse" $ do
      it "parses example input (length)" $
        bimap length length exampleInput `shouldBe` (11, 5)

      it "parses real input (length)" $
        bimap length length input `shouldBe` (515, 200)

    describe "part1" $ do
      it "computes accepted result for example input" $
        part1 exampleInput `shouldBe` 19114

      it "computes accepted result" $
        part1 input `shouldBe` 432788

    describe "part2" $ do
      it "computes accepted result for example input" $
        part2 exampleInput `shouldBe` 167409079868000

      it "computes accepted result" $
        part2 input `shouldBe` 142863718918201
