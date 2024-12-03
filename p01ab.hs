-- Advent of Code 2024 - Day 1 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The total distance between the lists:     2031679
-- Their similarity score:                  19678534
--
-- (cl) by Arno Jacobs, 2024-12-01

module AoC2024d01ab where

import Data.Char (isDigit)
import Data.List (sort)

-- Some initials
filename :: String
filename = "data/inputDay01_2024.txt"


parseNumbers :: [String] -> ([Int],[Int])
parseNumbers = twoSortedLists . map (\xs -> (numberLeft xs, numberRight xs)) 
    where 
        numberLeft  = read . takeWhile isDigit
        numberRight = read . dropWhile isDigit
    --  twoSortedLists :: [(Int,Int)] -> ([Int],[Int])
        twoSortedLists ps = (sortedLeft ps,sortedRight ps)
            where
                sortedLeft  = sort . map fst
                sortedRight = sort . map snd

partOne :: [String] -> Int
partOne = sum . deltaLists . parseNumbers

deltaLists :: ([Int],[Int]) -> [Int]
deltaLists (xs,ys) = map (\(x,y) -> abs(x-y)) $ zip xs ys

partTwo :: [String] -> Int
partTwo = sum . tallyLists . parseNumbers

tallyLists :: ([Int],[Int]) -> [Int]
tallyLists (xs,ys) = map tally xs      
    where
        tally x = x * length (filter (==x) ys)


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 1  (Haskell)"
            day1 <- lines <$> readFile filename
            putStr "Part one: The total distance between the lists:  "
            print $ partOne day1
            putStr "Part two: Their similarity score:               "
            print $ partTwo day1            
            putStrLn "0K.\n"


-- End of code
