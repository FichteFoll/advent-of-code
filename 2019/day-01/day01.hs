parse :: String -> [Int]
parse = map read . lines

part1 :: [Int] -> Int
part1 = sum . map cost

cost :: Int -> Int
cost n = n `div` 3 - 2

part2 :: [Int] -> Int
part2 = sum . map costRec

costRec :: Int -> Int
costRec n
    | m <= 0 = 0
    | otherwise = m + costRec m
    where m = cost n

main :: IO ()
main = do
    input <- parse <$> getContents
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ (show $ part2 input)
