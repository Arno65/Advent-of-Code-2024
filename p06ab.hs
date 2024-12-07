-- Advent of Code 2024 - Day 6 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: Number of distinct positions:  4988
--  Part two: Number of different positions: 1697
--
-- (cl) by Arno Jacobs, 2024-12-06

-- Hard work for this Haskell code
-- Even compiled the second part needs > 5 minutes on my M1 laptop
module AoC2024d06ab where

-- Some initials
filename :: String
filename = "data/inputDay06_2024.txt"

data Direction = North | East | South | West 
                    deriving (Show,Eq)

type Grid       = [String]
type Location   = (Int,Int)

-- My input data has ^ as guard, so start direction is North
cGuard          = '^' :: Char
cObstruction    = '#' :: Char
cPath           = '.' :: Char

-- quick sort but only store unique elements
-- So: [2,4,3,1,1,2,3,5,2,6,3,4] -> [1,2,3,4,5,6]
usort :: Ord a => [a] -> [a]
usort []     = []
usort (e:rl) = usort smaller ++ [e] ++ usort bigger
                where
                    smaller = filter (<e) rl
                    bigger  = filter (>e) rl 

getStartPosition :: Grid -> Location
getStartPosition grid = (snd guardSpot, snd guardLine)
    where
        guardLine   = head $ filter (\(yGrid,_) -> elem cGuard yGrid) (zip grid [0..])
        guardSpot   = head $ filter (\(xGrid,_) -> cGuard == xGrid) (zip (fst guardLine) [0..])

getGridSize :: Grid -> Location
getGridSize []      = (0,0)
getGridSize ([]:_)  = (0,0)
getGridSize grid    = (length (head grid), length grid)

-- Lab guards in 1518 follow a very strict patrol protocol which involves 
-- repeatedly following these steps:
--  1 -> If there is something directly in front of you, turn right 90 degrees.
--  2 -> Otherwise, take a step forward.
--

-- Part one ------- ------- ------- ------- ------- ------- ------- ------- ------- ------- -------

turnRight90Degrees :: Direction -> Direction
turnRight90Degrees North    = East
turnRight90Degrees East     = South
turnRight90Degrees South    = West
turnRight90Degrees _        = North

oneStep :: Location -> Direction -> Location
oneStep (x,y) North = (x,y-1)
oneStep (x,y) East  = (x+1,y)
oneStep (x,y) South = (x,y+1)
oneStep (x,y) _     = (x-1,y)

walkTheWalk :: Grid -> Location -> Location -> Direction -> [Location]
walkTheWalk grid (mx,my) (gx,gy) direction 
    |   (nx < 0) || (nx >= mx) 
    ||  (ny < 0) || (ny >= my)      = [(gx,gy)]     -- Leaving the grid, end of walkies. 
    |   contents == cObstruction    = walkTheWalk grid (mx,my) (gx,gy) (turnRight90Degrees direction)
    |   otherwise                   = [(gx,gy)] ++ walkTheWalk grid (mx,my) (nx,ny) direction
        where
            (nx,ny)     = oneStep (gx,gy) direction
            contents    = grid !! ny !! nx
            oneTurn     = turnRight90Degrees direction

partOne :: Grid -> Int
partOne grid = length $ usort $ walkTheWalk grid mxy gxy North
    where
        gxy = getStartPosition grid
        mxy = getGridSize grid


-- Part two ------- ------- ------- ------- ------- ------- ------- ------- ------- ------- -------

-- NO checking on previous content
placeObstruction :: Grid -> Location -> Grid
placeObstruction grid (x,y) =   take y grid
                            ++  [   take x line
                            ++          [cObstruction]
                            ++      drop (x+1) line]
                            ++  drop (y+1) grid
    where
        line = grid !! y

walkTheCircle :: Grid -> Location -> Location -> Location -> Direction -> [(Location,Direction)] -> Location
walkTheCircle grid (mx,my) (ox,oy) (gx,gy) direction locAndDir
    |   (nx < 0) || (nx >= mx) 
    ||  (ny < 0) || (ny >= my)      = (-1,1)    -- Leaving the grid, end of walkies. 
    |   elem gxyD locAndDir         = (ox,oy)   -- Walking in circles
    |   contents == cObstruction    = walkTheCircle grid (mx,my) (ox,oy) (gx,gy) (turnRight90Degrees direction) locAndDir
    |   otherwise                   = walkTheCircle grid (mx,my) (ox,oy) (nx,ny) direction (locAndDir ++ [gxyD])
        where
            gxyD            = ((gx,gy),direction)
            (nx,ny)         = oneStep (gx,gy) direction
            contents        = grid !! ny !! nx

partTwo :: Grid -> Int
partTwo grid = 
    length $ filter (\(x,_) -> x >= 0) [ walkTheCircle obsGrid (mx,my) (x,y) gxy North [] | 
        (x,y) <- freePath,
        let obsGrid = placeObstruction grid (x,y) ]
            where
                gxy         = getStartPosition grid
                (mx,my)     = getGridSize grid
                freePath    = usort $ tail $ walkTheWalk grid (mx,my) gxy North

main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 6  (Haskell)"
            day6 <- lines <$> readFile filename
            putStr "Part one: Number of distinct positions:  "
            print $ partOne day6
            putStr "Part two: Number of different positions: "
            print $ partTwo day6
            putStrLn "0K.\n"


--  End of code
