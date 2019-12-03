module MainSpec where

import Test.Hspec
import Main hiding (main)

main :: IO ()
main = do
  -- input <- parse <$> readFile "input.txt"
  hspec $ do
    describe "parse" $ do
      it "parses input" $
        parse "R8,U5,L5,D3\nU7,R6,D4,L4" `shouldBe`
          [[Pt 8 0,Pt 0 (-5),Pt (-5) 0,Pt 0 3],[Pt 0 (-7),Pt 6 0,Pt 0 4,Pt (-4) 0]]

    describe "used" $ do
      it "determines used fields 1" $
        used (Pt 0 0) [Pt 8 0] `shouldBe` [Pt i 0 | i <- [1..8]]
      it "determines used fields 2" $
        used (Pt 0 0) [Pt 0 (-3)] `shouldBe` [Pt 0 i | i <- [(-3)..(-1)]]
      it "determines used fields with sequence" $
        used (Pt 0 0) [Pt 0 1, Pt 1 0] `shouldBe` [Pt 0 1, Pt 1 1]

    describe "part 1" $ do
      it "computes simple example" $
        let input = parse "R8,U5,L5,D3\nU7,R6,D4,L4" in
        part1 input `shouldBe` 6

    describe "part 1" $ do
      it "computes complex example 1" $
        let input = parse "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" in
        part1 input `shouldBe` 159

    describe "part 1" $ do
      it "computes complex example 2" $
        let input = parse "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" in
        part1 input `shouldBe` 135
