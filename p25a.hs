-- Advent of Code 2024 - Day 25 part One
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: the number of unique matching lock/key pairs is: 3338

--
-- (cl) by Arno Jacobs, 2024-12-25

-- 
module AoC2025d25ab where

import Data.List        (transpose)
import Data.List.Split  (splitOn)

-- Some initials
filename :: String
filename = "data/inputDay25_2024.txt"

getShape :: [String] -> [Int]
getShape = map (length . filter (=='#')) . transpose

checkFit :: [Int] -> [Int] -> Bool
checkFit key1 key2 = [] == overlap
    where
        overlap = filter (>7) $ map (\(k1,k2) -> k1+k2) $ zip key1 key2

countFits :: [[Int]] -> [[Int]] -> Int
countFits keys locks =
    length [ 1 | key <- keys, lock <- locks, checkFit key lock ]

partOne :: [[String]] -> Int
partOne lines = countFits keys locks
    where
        keys    = map getShape $ filter isKey  lines
        locks   = map getShape $ filter isLock lines
        isKey key   = head key  == "#####"
        isLock lock = head lock == "....."

main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 25  (Haskell)"
            day25 <- splitOn [""]  <$> lines <$> readFile filename
            putStr "Part one: the number of unique matching lock/key pairs is: "
            print $ partOne day25
            putStrLn "0K.\n"


-- end of code
