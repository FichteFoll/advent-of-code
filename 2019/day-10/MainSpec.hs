module MainSpec where

import Test.Hspec
import Test.QuickCheck hiding (output)
import Data.List.Split (chunksOf)

import Main hiding (main)

main :: IO ()
main = do
  input <- parse <$> readFile "input.txt"
  hspec $ do
    let example = parse $ unlines [".#..##.###...#######","##.############..##.",".#.######.########.#",".###.#######.####.#.","#####.##.#.##.###.##","..#####..#.#########","####################","#.####....###.#.#.##","##.#################","#####.##.###..####..","..######..##.#######","####.##.####...##..#",".#####..#.######.###","##...#.##########...","#.##########.#######",".####.#.###.###.#.##","....##.##.###..#####",".#.#.###########.###","#.#.#.#####.####.###","###.##.####.##.#..##"]
        -- can't break lines here for some reason
        -- where example = unlines
        --   [".#..##.###...#######"
        --   ,"##.############..##."
        --   ,".#.######.########.#"
        --   ,".###.#######.####.#."
        --   ,"#####.##.#.##.###.##"
        --   ,"..#####..#.#########"
        --   ,"####################"
        --   ,"#.####....###.#.#.##"
        --   ,"##.#################"
        --   ,"#####.##.###..####.."
        --   ,"..######..##.#######"
        --   ,"####.##.####...##..#"
        --   ,".#####..#.######.###"
        --   ,"##...#.##########..."
        --   ,"#.##########.#######"
        --   ,".####.#.###.###.#.##"
        --   ,"....##.##.###..#####"
        --   ,".#.#.###########.###"
        --   ,"#.#.#.#####.####.###"
        --   ,"###.##.####.##.#..##"
        --   ]

    describe "parse" $ do
      it "parses example input" $
        parse ".#..#\n.....\n#####\n....#\n...##" `shouldBe`
          [(1,0),(4,0),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(3,4),(4,4)]

    describe "monitor" $ do
      it "finds correct result for example" $
        monitor example `shouldBe` ((11.0,13.0),210)

    describe "part1" $ do
      it "computes accepted result" $
        part1 input `shouldBe` 221

    describe "part2" $ do
      it "computes example" $
        part2 example `shouldBe` 802
      it "computes accepted result" $
        part2 input `shouldBe` 806
