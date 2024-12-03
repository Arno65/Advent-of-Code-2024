-- Advent of Code 2024 - Day 3 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: sum of all the results of the multiplications: 159892596
-- Part two: sum of all the results of the multiplications:  92626942
--
-- (cl) by Arno Jacobs, 2024-12-03

module AoC2024d03ab where

import Data.Char (isDigit)

-- Some initials
filename :: String
filename = "data/inputDay03_2024.txt"

-- Part one test result: 161
-- Part two test result:  48
testLine :: String
testLine = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"


sMultp  = "mul("    :: String
sDo     = "do()"    :: String
sDont   = "don't()" :: String
cComma  = ','       :: Char
cBacket = ')'       :: Char

getSumProducts :: String -> Bool -> Int
getSumProducts ps check_do = parse ps True check_do 

-- parse for part one and two
parse :: String -> Bool -> Bool -> Int
parse [] _       _          = 0
parse ps do_mult check_do   
    | do_mult  && isHead sMultp ps  = pp +  parse nps do_mult check_do
    | check_do && isHead sDo    ps  =       parse nps True    check_do
    | check_do && isHead sDont  ps  =       parse nps False   check_do
    | otherwise                     =       parse nps do_mult check_do
        where 
            nps = tail ps
            pp  = pairProduct $ drop 4 ps
        --  isHead :: Eq a => [a] -> [a] -> Bool
            isHead subs s = subs == take (length subs) s
        --  pairProduct :: String -> Int
            pairProduct subs    
                | ps1 == [] || ps2 == [] || nc1 || nb2  = 0
                | otherwise                             = (read ps1) * (read ps2) 
                    where
        --  Both Ints are always positive (otherwise check for '+' and '-' signs)
                        ps1 = takeWhile isDigit subs
                        ps2 = (takeWhile isDigit . tail . dropWhile isDigit) subs
                        nc1 = cComma  /= head (drop (length ps1) subs)
                        nb2 = cBacket /= head (drop (length (ps1++[cComma]++ps2)) subs)

main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 3  (Haskell)"
            day3 <- readFile filename
            putStr "Part one: sum of all the results of the multiplications: "
            print $ getSumProducts day3 False
            putStr "Part two: sum of all the results of the multiplications:  "
            print $ getSumProducts day3 True
            putStrLn "0K.\n"

--  End of code
