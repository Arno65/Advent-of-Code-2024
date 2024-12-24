-- Advent of Code 2024 - Day 20 - only part one . . .
-- Solution in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: the number of cheats:    1530 
-- Part two: the number of cheats: 1033983
--
-- (cl) by Arno Jacobs, 2024-12-20
--         optimized:   2024-12-24
--

-- 
module AoC2024d20ab where

import Data.List (sort)

type Location   = (Int,Int)
type Locations  = [Location]

data Room       = Room {    location    :: Location, 
                            neighbours  :: [Location], 
                            steps       :: Int }    
                        deriving (Eq,Ord,Show)

type Maze       = [Room]
type Steps      = [(Int,Location)]  -- Only need steps from start at free location

-- Some initials
filename :: String
filename = "data/inputDay20_2024.txt"

range = 100 :: Int 

cFree   = '.' :: Char
cStart  = 'S' :: Char 
cEnd    = 'E' :: Char

-- Little helpers ---------------------------------------------------------
--
-- In the locations list the first element is the start location
-- and the second element is the end location
parseMaze :: [String] -> Locations
parseMaze [] = []
parseMaze ls = mazePath
    where
        maxx = length (head ls)
        maxy = length ls
        sp = getCharPosition ls cStart
        ep = getCharPosition ls cEnd
        freeSpace   = [ (x,y) | x <- [0..maxx-1], 
                                y <- [0..maxy-1],
                                ls!!y!!x == cFree ]
        mazePath = [sp,ep] ++ freeSpace

getCharPosition :: [String] -> Char -> Location
getCharPosition [] _    = (0,0) 
getCharPosition ls c 
    | xh == []  = (0,0)
    | otherwise = (x,y) 
   where
        y   = length $  takeWhile (not . elem c) ls
        xh  =           dropWhile (not . elem c) ls
        x   = length $  takeWhile (/=c) (xh!!0)

-- Maze helpers ----------------------------------------------------------------
--
convertLocationsToRooms :: Locations -> Maze
convertLocationsToRooms ls = map (createRoom ls) ls

createRoom :: Locations -> Location -> Room
createRoom ls xy = Room { location = xy, neighbours = nxyps, steps = -1 } 
    where 
        nxyps = [ nxy | nxy <- neighbours xy, elem nxy ls ] 
        neighbours (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

amazingSolver :: Locations -> Maze
amazingSolver [] = []
amazingSolver ls = workSteps (head ls) $ convertLocationsToRooms ls

-- Walk the maze - step by step
workSteps :: Location -> Maze -> Maze
workSteps start = workSteps' 0 . setStartLocation start
    where
    --  workSteps' :: Int -> Maze -> Maze
        workSteps' step maze    | nlNewSteps == []  = maze
                                | otherwise         = workSteps' (step+1) newMaze 
            where 
                startRooms  = filter (\room -> steps room == step) maze
                nln         = concat $ map neighbours startRooms
                nlRooms     = filter (\room -> elem (location room) nln) maze
                nlNewSteps  = filter (\room -> steps room == -1) nlRooms
                nlSteps0K   = filter (\room -> steps room /= -1) nlRooms
                restRooms   = filter (\room -> not (elem (location room) nln)) maze
                nlPlus1     = setSteps (step+1) nlNewSteps
                newMaze     = nlPlus1 ++ nlSteps0K ++ restRooms
                setSteps s  = map (setStep s) 
                setStep s r = Room { location = rl, neighbours = nl, steps = s }
                    where
                        rl  = location   r
                        nl  = neighbours r
                        
-- NO range checking !!!
setStartLocation :: Location -> Maze -> Maze
setStartLocation start maze =   [ Room {    location    = start, 
                                            neighbours  = nl, 
                                            steps       = 0  } ] 
                            ++  restRooms
    where
        startRooms  = filter (\room -> location room == start) maze 
        nl          = neighbours $ head startRooms
        restRooms   = filter (\room -> location room /= start) maze
        
-- Work the maze and create the steps-dataset ---------------------------------
--
workMaze :: [String] -> Steps
workMaze = roomsToSteps . amazingSolver . parseMaze 

roomsToSteps :: Maze -> Steps
roomsToSteps = sort . map (\room -> (steps room, location room)) 

-- Cheat code ------------------------------------------------------------------
--
manhattan :: Location -> Location -> Int
manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

cheat :: Steps -> Int -> [Int]
cheat ((s,l):rsls) maxDistance 
    | rsls == []    = []
    | otherwise     = deltas ++ cheat rsls maxDistance
    where
        deltas = [ cheat - md | 
                    (s',l') <- rsls,
                    let md = manhattan l l',
                    md <= maxDistance,
                    let cheat = abs (s - s'),
                    cheat > md ]

cheatMaze :: Steps -> Int -> Int
cheatMaze steps = length . filter (>=range) . (cheat steps)


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 20  (Haskell)"
            day20 <- workMaze <$> lines <$> readFile filename
            putStr "Part one: the number of cheats:    "   
            print $ cheatMaze day20 2
            putStr "Part two: the number of cheats: "   
            print $ cheatMaze day20 20
            putStrLn "0K.\n"

-- End of code

