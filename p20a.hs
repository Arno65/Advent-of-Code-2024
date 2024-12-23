-- Advent of Code 2024 - Day 20 - only part one . . .
-- Solution in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: the number of cheats:    1530 
--
-- (cl) by Arno Jacobs, 2024-12-20
--         optimized:   2024-12-23
--

-- 
module AoC2024d20ab where

import Data.List.Split (splitOn)
import Data.List (sort)

type Direction  = (Int,Int)
type Location   = (Int,Int)
type Locations  = [Location]

data Room       = Room {    location    :: Location, 
                            neighbours  :: [Location], 
                            steps       :: Int }    
                        deriving (Eq,Ord,Show)

type Maze       = [Room]

-- Some initials
filename :: String
filename = "data/inputDay20_2024.txt"

range = 100 :: Int 

dUp     = ( 0,-1) :: Direction
dRight  = ( 1, 0) :: Direction
dDown   = ( 0, 1) :: Direction
dLeft   = (-1, 0) :: Direction

-- Posible shortcuts or cheats in a maze
shortcuts :: [(Direction,Direction)] 
shortcuts = [   (dUp,   dRight),
                (dDown, dRight),
                (dUp,   dLeft ),
                (dDown, dLeft ),
                (dUp,   dDown ),
                (dLeft, dRight) ]

cFree   = '.' :: Char
cStart  = 'S' :: Char 
cEnd    = 'E' :: Char

-- Little helpers ---------------------------------------------------------
--
stripBorders :: [String] -> [String]
stripBorders = map (tail . init) . (tail . init) 

locationBorders :: Locations -> Location
locationBorders lxys = (maximum (map fst lxys), maximum (map snd lxys))

addPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
addPair (x1,y1) (x2,y2) = (x1+x2,y1+y2)

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

cheatMaze :: Locations -> [Int]
cheatMaze ls = sort [ evaluateCheat rooms cheat mazeSteps | cheat <- blocks ]
    where
        (bx,by)     = locationBorders ls
        rooms       = amazingSolver ls
        mazeSteps   = steps (head (getRooms rooms (ls!!1)))
        -- The blocks can be less
        -- Only remove block is at least two paths are bordering
        blocks      = [ (x,y) | x <- [0..bx], y <- [0..by], 
                                not (elem (x,y) ls),
                                let cUp     = if elem (addPair (x,y) dUp)    ls then 1 else 0,
                                let cRight  = if elem (addPair (x,y) dRight) ls then 1 else 0,
                                let cDown   = if elem (addPair (x,y) dDown)  ls then 1 else 0,
                                let cLeft   = if elem (addPair (x,y) dLeft)  ls then 1 else 0,
                                let pathCnt = cUp + cRight + cDown + cLeft,
                                pathCnt > 1 ]

evaluateCheat :: Maze -> Location -> Int -> Int
evaluateCheat rooms cxy mazeSteps = 
    maximum [ cheat | 
                ix <- [0..length shortcuts -1],
                let (dxy1,dxy2) = shortcuts!!ix,
                let checkPoint1 = addPair cxy dxy1,
                let rl1 = getRooms rooms checkPoint1,
                rl1 /= [],
                let checkPoint2 = addPair cxy dxy2,
                let rl2 = getRooms rooms checkPoint2,
                rl2 /= [],
                let mazesteps1 = steps (head rl1),
                let mazesteps2 = steps (head rl2),
                let cheat = abs (mazesteps1 - mazesteps2) - 2 ]


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
        

-- Helper code for 'plotting' byte drops ---------------------------------------
-- 
plotByteDrops :: Locations -> IO ()
plotByteDrops bytes =
    printSList [[ plotChar b
                    | x <- [0..bx], 
                        let b = elem (x,y) bytes ]
                    | y <- [0..by]]
        where
            (bx,by) = locationBorders bytes
            plotChar True   = '.'
            plotChar _      = '#'
            -- 
            printSList []     = do  return ()
            printSList (l:ls) = do  putStrLn l
                                    printSList ls


-- part one --------------------------------------------------------------------
-- 
partOne :: [String] -> Int
partOne = length . filter (>=range) . cheatMaze . parseMaze . stripBorders


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 20  (Haskell)"
            day20 <- lines <$> readFile filename
            putStr "Part one: the number of cheats: "   
            print $ partOne day20

            putStrLn "0K.\n"

-- End of code

