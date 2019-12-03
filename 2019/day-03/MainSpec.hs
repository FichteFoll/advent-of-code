module MainSpec where

import Test.Hspec
import qualified Data.Set as Set

import Main hiding (main)


main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    describe "parse" $ do
      it "parses input" $
        parse "R8,U5,L5,D3\nU7,R6,D4,L4" `shouldBe`
          [[('R',8),('U',5),('L',5),('D',3)],[('U',7),('R',6),('D',4),('L',4)]]

    describe "used" $ do
      let used' = used (Pt 0 0) . head . parse
      it "determines used fields right" $
        used' "R8" `shouldBe` Set.fromList [Pt i 0 | i <- [1..8]]
      it "determines used fields up" $
        used' "U3" `shouldBe` Set.fromList [Pt 0 i | i <- [(-3)..(-1)]]
      it "determines used fields from sequence" $
        used' "D1,R1,U1" `shouldBe` Set.fromList [Pt 0 1, Pt 1 1, Pt 1 0]

    describe "steps" $ do
      let steps' target cmds = steps target (head $ parse cmds)
      it "counts required steps 1" $
        steps' (Pt 0 6) "D8" `shouldBe` 6

      it "counts required steps 2" $
        steps' (Pt 1 1) "D8,R1,U8" `shouldBe` 16

    let example1 = parse "R8,U5,L5,D3\nU7,R6,D4,L4"
        example2 = parse "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
        example3 = parse "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

    describe "part 1" $ do
      it "computes simple example 1" $
        part1 example1 `shouldBe` 6

      it "computes complex example 2" $
        part1 example2 `shouldBe` 159

      it "computes complex example 3" $
        part1 example3 `shouldBe` 135

    describe "part 2" $ do
      it "computes simple example 1" $
        part2 example1 `shouldBe` 30

      it "computes complex example 2" $
        part2 example2 `shouldBe` 610

      it "computes complex example 3" $
        part2 example3 `shouldBe` 410

    describe "part1" $ do
      it "computes accepted result" $
         part1 input `shouldBe` 232

    describe "part2" $ do
      it "computes accepted result" $
         part2 input `shouldBe` 6084
