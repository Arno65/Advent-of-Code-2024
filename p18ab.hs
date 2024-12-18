-- Advent of Code 2024 - Day 18 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--- Part one: the minimum number of steps needed to reach the exit: 314
--  Part two: the coordinates of the first byte that will prevent
--    the exit from being reachable from the starting position are: (15,20)
--
-- (cl) by Arno Jacobs, 2024-12-18

-- 
module AoC2024d18ab where

import Data.List.Split (splitOn)

type Location   = (Int,Int)
type Locations  = [Location]

data Room       = Room {    location    :: Location, 
                            neighbours  :: [Location], 
                            steps       :: Int }    
                        deriving (Eq,Ord,Show)

type Maze       = [Room]


-- Some initials
filename :: String
filename = "data/inputDay18_2024.txt"

-- The memory space is a square, only declare one size
memorySpace :: Int
memorySpace = 70

bytesOne :: Int
bytesOne = 1024

start :: Location
start = (0,0)

endGoal :: Location
endGoal = (memorySpace,memorySpace)


-- Little parse helper ---------------------------------------------------------
--
coordinate :: String -> Location
coordinate s = (read sx, read sy)
    where
        (sx:sy:_) = splitOn "," s

-- Maze helpers ----------------------------------------------------------------
--
convertLocationsToRooms :: Locations -> Maze
convertLocationsToRooms ls = map (createRoom ls) ls

createRoom :: Locations -> Location -> Room
createRoom ls xy = Room { location = xy, neighbours = nxyps, steps = -1 } 
    where 
        nxyps = [ nxy | nxy <- neighbours xy, elem nxy ls ] 
        neighbours (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

getRooms :: Maze -> Location -> Maze
getRooms mrs l = filter (\room -> (location room) == l ) mrs

amazingSolver :: Locations -> Maze
amazingSolver ls = workSteps start $ convertLocationsToRooms ls

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
        
-- Solve the maze ----------------------------------------------------------------
--
partOne :: Locations -> Int
partOne bytes   | endData == [] = -1 
                | otherwise     = steps (head endData)
    where
        bytesPart   = take bytesOne bytes
        freeSpace   = [ (x,y) | x <- [0..memorySpace], 
                                y <- [0..memorySpace],
                                not (elem (x,y) bytesPart) ]
        sm          = amazingSolver freeSpace
        endData     = getRooms sm endGoal

-- Looking for the first byte to drop that will block the exit from the start position
binarySearch :: Int -> Int -> Locations -> Location
binarySearch low high bytes
    | lastSteps && s > 0    = last $ take high bytes
    | lastSteps             = last $ take low  bytes
    | s > 0                 = binarySearch middlePoint high        bytes
    | otherwise             = binarySearch low         middlePoint bytes
        where
            lastSteps   = (high-low) < 2
            middlePoint = low + div (high-low) 2
            bytesPart   = take middlePoint bytes
            freeSpace   = [ (x,y) | x <- [0..memorySpace], 
                                    y <- [0..memorySpace],
                                    not (elem (x,y) bytesPart) ]
            sm          = amazingSolver freeSpace
            endData     = getRooms sm endGoal
            s           = (steps . head . getRooms sm) endGoal

-- Work with a binary search on the bytes length -------------------------------
--
partTwo :: Locations -> Location
partTwo bytes = binarySearch bytesOne (length bytes) bytes


-- Helper code for 'plotting' byte drops ---------------------------------------
-- 
plotByteDrops :: Locations -> Int -> IO ()
plotByteDrops bytes size = 
    printSList [[ plotChar b
                    | x <- [0..size], 
                        let b = elem (x,y) bytes ]
                    | y <- [0..size]]
        where
            plotChar True   = '#'
            plotChar _      = '.'
            printSList []     = do  return ()
            printSList (l:ls) = do  putStrLn l
                                    printSList ls


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 18  (Haskell)"
            day18 <- map coordinate <$> lines <$> readFile filename
            putStr "Part one: the minimum number of steps needed to reach the exit: "
            print $ partOne day18
            putStrLn "Part two: the coordinates of the first byte that will prevent"
            putStr "  the exit from being reachable from the starting position are: " 
            print $ partTwo day18
            putStrLn "0K.\n"


-- End of code
