-- Advent of Code 2024 - Day 13 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: The fewest tokens spend to win all possible prizes:           26810
--  Part two: The fewest tokens spend to win all possible prizes: 108713182988244
--
-- (cl) by Arno Jacobs, 2024-12-13

-- 
module AoC2024d13ab where

import Data.Char (isDigit)

type Pair       = (Int,Int)
type Button     = Pair
type Prize      = Pair
type Machine    = (Button,Button,Prize)
type Machines   = [Machine]


-- Some initials
filename :: String
filename = "data/inputDay13_2024.txt"


parseMachines :: [String] -> Machines
parseMachines []    = []
parseMachines lines | length lines < 3  =   []
                    | otherwise         =   [(buttonA,buttonB,prize)] 
                                        ++  parseMachines (drop 4 lines)
    where
        buttonA     = (numberPair . head . filter (starts "Button A")) lines
        buttonB     = (numberPair . head . filter (starts "Button B")) lines
        prize       = (numberPair . head . filter (starts "Prize"))    lines


starts :: String -> String -> Bool
starts subs s = subs == take (length subs) s

numberPair :: String -> Pair
numberPair line = (read ns1, read ns2)
    where
        part1   = dropWhile (not . isDigit) line
        ns1     = takeWhile isDigit part1
        part2   = dropWhile (not . isDigit) $ dropWhile isDigit part1
        ns2     = takeWhile isDigit part2

-- Prize for using a button - fst is A 3 tokens, snd is B 1 token
tokens :: Pair -> Int
tokens (a,b) = 3*a+b

addPrize :: Machine -> Machine
addPrize (buttonA,buttonB,(px,py)) = (buttonA,buttonB,(px+i,py+i))
    where
        i = 10000000000000 -- increase prizes by 10^13

increasePrizes :: Machines -> Machines
increasePrizes = map addPrize 

solveMachine :: Machine -> Pair
solveMachine ((ax,ay),(bx,by),(px,py)) 
    | dv == 0       = (0,0)     -- division by 0? zero score
    | chkSolution   = (a,b)
    | otherwise     = (0,0)     -- Zero score for non-integer solution
        where
            nm  = by*px-bx*py           -- From linear algebra
            dv  = by*ax-bx*ay           -- Gaussian elimination
            a   = div nm dv             -- dv could be 0, then zero score
            b   = div (px - a*ax) bx
            chkSolution =   (a*ax + b*bx == px)     -- check the integer solution
                        &&  (a*ay + b*by == py)
    
sumTokens :: Machines -> Int
sumTokens = sum . map (tokens . solveMachine) 


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 13 (Haskell)"
            day13 <- parseMachines <$> lines <$> readFile filename
            putStr "Part one: fewest tokens spend to win all possible prizes:           "
            print $ sumTokens day13
            putStr "Part two: fewest tokens spend to win all possible prizes: "
            print $ sumTokens $ increasePrizes day13
            putStrLn "0K.\n"

--  End of code
