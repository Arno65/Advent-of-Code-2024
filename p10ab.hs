-- Advent of Code 2024 - Day 10 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: Sum of scores of all trailheads:   531
--  Part two: Sum of ratings of all trailheads: 1210
--
-- (cl) by Arno Jacobs, 2024-12-10

module AoC2024d10ab where

import Data.List (nub,singleton)
-- nub:         collect only unique elements from a list by removing all duplicate elements
-- singleton:   create a list from a single element, or a String from a single Char

-- Some initials
filename :: String
filename = "data/inputDay10_2024.txt"

type Location   = (Int,Int)
type Height     = (Int,Location)
type Topo       = [[Location]]

data Path p     = EndPoint p | P p [Path p]     deriving (Show,Eq)

heights :: String -> [[Int]]
heights = map (map (read . singleton)) . lines 

heightsList :: [[Int]] -> [Height]
heightsList hls = [ (hls !! y !! x,(x,y)) | x <- [0..mxx-1], y <- [0..mxy-1]]
    where
        (mxx,mxy) = (length (head hls), length hls) -- grid size

-- Collect locations per height 
tally :: [Height] -> Topo
tally hl = [ map snd ths | height <- [0..9], let ths = filter ((== height) . fst) hl ]

topo :: String -> Topo
topo = tally . heightsList . heights

-- Create all trails from a given start location 
trail :: Topo -> Location -> Int -> Path Location
trail _     xy   9      = EndPoint xy   -- maximum trail, from 0 to 9
trail topo (x,y) height 
    | nextSteps == []   = P (x,y) []    -- Not a complete hiking trail
    | otherwise         = P (x,y) [ trail topo ns (height+1) | ns <- nextSteps ]
        where 
            oneSteps        = [(x-1,y),(x,y+1),(x+1,y),(x,y-1)] -- N,E,S,W 
            nextLocations   = topo !! (height+1)
            nextSteps       = [ nl | nl <- nextLocations, elem nl oneSteps ]


-- Part one ----------------------------------------------------------------------------------------
distinctTrailheadsScore :: Topo -> Int
distinctTrailheadsScore topo = sum [ distinctScore (trail topo startPosition 0) | 
                                    startPosition <- head topo ]

distinctScore :: Path Location -> Int
distinctScore = length . nub . endPoints    -- nub -> uniques from list for distinct paths

endPoints :: Path Location -> [Path Location]
endPoints (EndPoint xy) = [EndPoint xy]
endPoints (P _ paths)   = concat [ endPoints p | p <- paths ]


-- Part two ----------------------------------------------------------------------------------------
allTrailheadsScore :: Topo -> Int
allTrailheadsScore topo = sum [ scorePath (trail topo startPosition 0) | 
                                    startPosition <- head topo ]

-- ratings for all trailheads 
scorePath :: Path Location -> Int
scorePath (EndPoint _)  = 1
scorePath (P _ paths)   = sum [ scorePath p | p <- paths ]


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 10 (Haskell)"
            day10 <- topo <$> readFile filename
            putStr "Part one: Sum of scores of all trailheads:   "
            print $ distinctTrailheadsScore day10
            putStr "Part two: Sum of ratings of all trailheads: "
            print $ allTrailheadsScore day10
            putStrLn "0K.\n"

--  End of code
