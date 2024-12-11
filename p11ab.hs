-- Advent of Code 2024 - Day 11 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: number of stones:          189092
--  Part two: number of stones: 224869647102559
--
-- (cl) by Arno Jacobs, 2024-12-11

-- module AoC2024d11ab where

import Data.List.Split  (splitOn)

type Mem    = ((Int,Int),Int)    -- ((stone,blink),value)
type Memory = [Mem]

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
splitEven n = ( read nsl, read nsr )
    where
        ns          = show n
        hnsl        = div (length ns) 2
        (nsl,nsr)   = (take hnsl ns, drop hnsl ns)

fromMemory :: (Int,Int) -> Memory -> (Bool,Int)
fromMemory sb memory    | hits == []    = (False,0) 
                        | otherwise     = (True,value)
    where
        hits        = filter (\(sbm,_) -> sbm == sb) memory
        (_,value)   = head hits

updateMemory :: Int -> Int -> Int -> Memory -> Memory
updateMemory stone blink value memory   | inMemory  = memory
                                        | otherwise = memory ++ [((stone,blink),value)]
    where
--        (inMemory,_) = fromMemory (stone,blink) memory
        inMemory = elem (stone,blink) $ map fst memory

solve :: Int -> Int -> Memory -> (Int,Memory)
solve _     0     memory    = (1,memory) 
solve stone blink memory 
    | inMemory              = (value,memory) 
    | stone == 0            = (value0,    updateMemory 1     nBlink value0    memory0) 
    | evenDigits stone      = (vLR,       updateMemory stone  blink vLR       memoryLR)
    | otherwise             = (value2024, updateMemory s2024 nBlink value2024 m2024) 
        where
            nBlink                  = blink - 1
            (inMemory,value)        = fromMemory (stone,blink) memory
            (value0,memory0)        = solve 1 nBlink memory
            (leftStone,rightStone)  = splitEven stone
            (vLeft,mLeft)           = solve leftStone  nBlink memory
            (vRight,mRight)         = solve rightStone nBlink (updateMemory leftStone nBlink vLeft mLeft)
            memoryLR                = updateMemory rightStone nBlink vRight mRight
            vLR                     = vLeft + vRight
            s2024                   = stone * 2024
            (value2024,m2024)       = solve s2024 nBlink memory
            
countStones :: Int -> [Int] -> Int 
countStones blinks = sum . map (fst . (\stone -> solve stone blinks []))


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 11 (Haskell)"
            day11 <- numbers <$> readFile filename
            putStr "Part one: number of stones: "
            print $ countStones counts1 day11
            putStr "Part two: number of stones: "
            print $ countStones counts2 day11   --- still slow 7m49s
            putStrLn "0K.\n"


--  End of code


