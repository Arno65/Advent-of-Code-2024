-- Advent of Code 2024 - Day 7 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: The total calibration result:   1260333054159
--  Part two: The total calibration result: 162042343638683
--
-- (cl) by Arno Jacobs, 2024-12-07

-- Compiled the code needs > 32 seconds to run on my M1.
-- module AoC2024d07ab where

import Data.Char (isDigit)

data Operator = Add | Multiply | Concat  deriving (Show,Eq)

-- Some initials
filename :: String
filename = "data/inputDay07_2024.txt"

operatorsList :: [Operator]
operatorsList = [Add,Multiply,Concat]

-- Extra operator for this task
-- Concat two Ints, like 123 +++ 79 becomes 12379
(+++) :: Int -> Int -> Int
(+++) i1 i2 = read (show i1 ++ show i2)

-- Text data to Ints
getNumbers :: String -> (Int,[Int])
getNumbers xs = (number1, getInts rns)
    where
        number1 = read $ takeWhile (/= ':') xs
        rns     = dropWhile (/= ' ') xs

getInts :: String -> [Int]
getInts []       = []
getInts (' ':ns) = getInts ns
getInts ns       = [read ns1] ++ getInts rns
    where
        ns1 = takeWhile isDigit ns
        rns = dropWhile isDigit ns


-- Part one and two ---------------------------------------------------------------------
--

-- Evaluate from left-to-right -- for all three operators
evaluate :: [Int] -> [Operator] -> Int
evaluate []     _   = 0
evaluate (n:[]) _   = n
evaluate ns     ops | ol == Add     = ne +   nl
                    | ol == Concat  = ne +++ nl
                    | otherwise     = ne *   nl
    where
        nl = last ns
        ni = init ns
        ol = last ops
        oi = init ops
        ne = evaluate ni oi

getOperators :: Int -> Int -> [Operator]
getOperators _    0 = []
getOperators nops n = [operatorsList !! m] ++ getOperators nops d
    where
        (d,m) = divMod n nops

allOperators :: Int -> [Int] -> [[Operator]]
allOperators nops ns = [ take n (getOperators nops i ++ filler) | i <- [0..nops^n-1]]
    where 
        n       = length ns - 1
        filler  = repeat $ head operatorsList

canCalibrate :: Int -> [Int] -> [[Operator]] -> Bool
canCalibrate cn rns = elem cn . map (evaluate rns) 

-- One calibrate to work them all...
calibrate :: Int -> [(Int,[Int])] -> Int
calibrate nops nss = sum [ cn | (cn,rns) <- nss, canCalibrate cn rns (allOperators nops rns) ]


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 7  (Haskell)"
            day7 <- map getNumbers <$> lines <$> readFile filename
            putStr "Part one: The total calibration result:   "
            print $ calibrate 2 day7
            putStr "Part two: The total calibration result: "
            print $ calibrate 3 day7
            putStrLn "0K.\n"

--  End of code
