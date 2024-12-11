-- Advent of Code 2024 - Day 11 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: number of stones:          189092
--  Part two: number of stones: 224869647102559
--
-- Change my code 4 times for speed improvement
-- From an endless run to 8 minutes to 8, 4.25, 3.7 seconds on my M1.
--
-- (cl) by Arno Jacobs, 2024-12-11

-- module AoC2024d11ab where

import Data.List.Split  (splitOn)

type Mem    = (Int,Int)     -- (stone,value)
type Memory = [[Mem]]       -- index !! blink 

-- Some initials
filename :: String
filename = "data/inputDay11_2024.txt"

counts1, counts2 :: Int
counts1 = 25
counts2 = 75

numbers :: String -> [Int]
numbers s = map read (splitOn " " s)

evenDigits :: Int -> Bool
evenDigits = even . length . show

splitEven :: Int -> (Int,Int)
splitEven n = divMod n (10^hnsl)        --  ~ 15% quicker than string conversion
    where
        hnsl = div (length (show n)) 2

-- Memoization helpers ---------------------------------------------------------
-- 
fromMemory :: Int -> Int -> Memory -> (Bool,Int)
fromMemory stone blink memory   | hits == []        = (False,0) 
                                | otherwise         = (True,snd (head hits))
    where
        hits = filter (\(ts,_) -> ts==stone) (memory !! blink)

updateMemory :: Int -> Int -> Int -> Memory -> Memory
updateMemory stone blink value memory   
    | inMemory  = memory
    | otherwise = take blink memory ++ [insert] ++ drop (blink+1) memory
    where
        inMemory    = elem stone $ map fst (memory !! blink)
        insert      = (memory !! blink) ++ [(stone,value)]  -- Add (stone,value) to blink-row

-- Count stone by stone --------------------------------------------------------
--
count :: Int -> Int -> Memory -> (Int,Memory)
count _     0     memory    = (1,memory) 
count stone blink memory 
    | inMemory              = (value,memory)    -- memoization
    | stone == 0            = (value0,     updateMemory 1     nBlink value0     memory0) 
    | evenDigits stone      = (vLeftRight, mLeftRight)
    | otherwise             = (value2024,  updateMemory s2024 nBlink value2024  m2024) 
        where
            nBlink                  = blink - 1
            (inMemory,value)        = fromMemory stone blink memory
            (value0,memory0)        = count 1 nBlink memory
            (leftStone,rightStone)  = splitEven stone
            (vLeft,mLeft)           = count leftStone  nBlink memory
            (vRight,mLeftRight)     = count rightStone nBlink mLeft
            vLeftRight              = vLeft + vRight
            s2024                   = stone * 2024
            (value2024,m2024)       = count s2024 nBlink memory

-- (In this Haskell code) 
-- Starting with a clear memory for every new stone is 1.5 times faster
countStones :: Int -> [Int] -> Int 
countStones blinks = sum . map (fst . (\stone -> count stone blinks emptyMemory))
    where 
        emptyMemory = replicate (blinks+1) []   -- create a list for every blink


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 11 (Haskell)"
            day11 <- numbers <$> readFile filename
            putStr "Part one: number of stones after "
            putStr $ show counts1
            putStr " blinks:          "
            print $ countStones counts1 day11
            putStr "Part two: number of stones after "
            putStr $ show counts2
            putStr " blinks: "
            print $ countStones counts2 day11   
            putStrLn "0K.\n"

--  End of code
