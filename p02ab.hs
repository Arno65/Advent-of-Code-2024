-- Advent of Code 2024 - Day 2 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- The number of safe reports is:           624
-- The number of safe dampened reports is:  658 
--
-- (cl) by Arno Jacobs, 2024-12-02

module AoC2024d02ab where

import Data.List.Split (splitOn)


-- Some initials
filename :: String
filename = "data/inputDay02_2024.txt"


partOne :: [[Int]] -> Int
partOne = length . filter (==True) . map isSafe

-- The levels are either all increasing or all decreasing.
-- Any two adjacent levels differ by at least one and at most three.
isSafe :: [Int] -> Bool
isSafe ls   =   isSafeIncreasing ls
            ||  isSafeIncreasing (reverse ls)

isSafeIncreasing :: [Int] -> Bool
isSafeIncreasing []        = True
isSafeIncreasing (_:[])    = True
isSafeIncreasing (l:ls)    
    | delta >= 1 && delta <= 3  = isSafeIncreasing ls
    | otherwise                 = False
        where
            delta  = head ls - l

isSafeDampened :: [Int] -> Bool
isSafeDampened ls =  or [ isSafe ls' | ix <- [0..length ls - 1], let ls' = stripElement ix ls ]
    where
        --  stripElement :: Int -> [Int] -> [Int]        
            stripElement ix ls = take ix ls ++ drop (ix+1) ls

partTwo :: [[Int]] -> Int
partTwo = length . filter (==True) . map isSafeDampened

numbers :: String -> [Int]
numbers s = map read (splitOn " " s)

main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 2  (Haskell)"
            day2 <- map numbers <$> lines <$> readFile filename
            putStr "Part one: The number of safe reports is:          "
            print $ partOne day2
            putStr "Part two: The number of safe dampened reports is: "
            print $ partTwo day2            
            putStrLn "0K.\n"

-- End of code
