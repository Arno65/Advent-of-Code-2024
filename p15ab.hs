-- Advent of Code 2024 - Day 15 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: the sum of all boxes' GPS coordinates is:       1421727
--  Part two: the sum of all boxes' final GPS coordinates is: 1463160
--
-- (cl) by Arno Jacobs, 2024-12-18

-- 
module AoC2024d15ab where

-- 
import Data.List (nub)

-- Some initials
filename :: String
filename = "data/inputDay15_2024.txt"

type Warehouse  = [String]
type Location   = (Int,Int)
type Direction  = (Int,Int)
type Locations  = [Location]
type Directions = [Direction]
type Box        = (Location,Location)
type Boxes      = [Box]

cWall   = '#' :: Char
cRobot  = '@' :: Char
cBox    = 'O' :: Char 
cFree   = '.' :: Char   -- only here for pretty printing
cSpace  = ' ' :: Char

-- For part two
cBoxLeft    = '[' :: Char 
cBoxRight   = ']' :: Char 

-- for reshape at part two
sWall   = "##" :: String
sRobot  = "@." :: String
sBox    = "[]" :: String
sFree   = ".." :: String

cUp     = '^' :: Char
cRight  = '>' :: Char
cDown   = 'v' :: Char
cLeft   = '<' :: Char

dUp     = ( 0,-1) :: Direction
dRight  = ( 1, 0) :: Direction
dDown   = ( 0, 1) :: Direction
dLeft   = (-1, 0) :: Direction
noMove  = ( 0, 0) :: Direction

addPair :: Direction -> Direction -> Direction
addPair (x1,y1) (x2,y2) = (x1+x2,y1+y2)

addPairPair :: Direction -> Box -> Box
addPairPair mxy (pl,pr) = (addPair mxy pl,addPair mxy pr)

warehouseSize :: Warehouse -> Location
warehouseSize xys = (length (head xys), length xys)

parse :: [String] -> ([Locations],Directions)
parse lines = (whls,moves)
    where
        warehouseLines  = takeWhile (/="") lines
        directionsLines = concat $ dropWhile (/="") lines
        wxy             = warehouseSize warehouseLines
        whls            = parseWarehouse warehouseLines (0,0) wxy [] [] []
        moves           = parseMoves directionsLines

parseWarehouse :: Warehouse -> Location -> Location -> Locations -> Locations -> Locations -> [Locations]
parseWarehouse whs (x,y) (wx,wy) rls bls wls
    | (y>=wy)               = [rls,bls,wls]
    | (x>=wx)               = parseWarehouse whs (0,y+1) (wx,wy) rls bls wls
    | whs!!y!!x == cBox     = parseWarehouse whs (x+1,y) (wx,wy) rls (bls++[(x,y)]) wls
    | whs!!y!!x == cRobot   = parseWarehouse whs (x+1,y) (wx,wy) (rls++[(x,y)]) bls wls
    | whs!!y!!x == cWall    = parseWarehouse whs (x+1,y) (wx,wy) rls bls (wls++[(x,y)])
    | otherwise             = parseWarehouse whs (x+1,y) (wx,wy) rls bls wls

parseMoves :: String -> Directions
parseMoves = map parseMove

parseMove :: Char -> Direction
parseMove cDirection    
    | cDirection == cUp     = dUp
    | cDirection == cRight  = dRight 
    | cDirection == cDown   = dDown 
    | cDirection == cLeft   = dLeft 
    | otherwise             = noMove


-- Part one ---------------------------------------------------------------------------
--

moveRobot :: Location -> Locations -> Locations -> Directions -> (Location,Locations)
moveRobot rxy boxes _     []        = (rxy,boxes)
moveRobot rxy boxes walls (mxy:rms)
    | hitWall   = moveRobot  rxy boxes walls rms 
    | hitBox    = moveRobot brxy nBxs  walls rms
    | otherwise = moveRobot nrxy boxes walls rms
        where
            nrxy        = addPair rxy mxy
            hitWall     = elem nrxy walls
            hitBox      = elem nrxy boxes
            (brxy,nBxs) = moveBoxAndRobot rxy boxes walls mxy

-- Robot hits box, can box or boxes be moved? And if yes, just do so.
moveBoxAndRobot :: Location -> Locations -> Locations -> Direction -> (Location,Locations)
moveBoxAndRobot rxy boxes walls mxy 
    | mxy == dUp    = moveBoxAndRobotUp    rxy boxes walls
    | mxy == dLeft  = moveBoxAndRobotLeft  rxy boxes walls
    | mxy == dRight = moveBoxAndRobotRight rxy boxes walls
    | mxy == dDown  = moveBoxAndRobotDown  rxy boxes walls
    | otherwise     = error " * Missing or illegal direction! * "

moveBoxAndRobotUp :: Location -> Locations -> Locations -> (Location,Locations)
moveBoxAndRobotUp (rx,ry) boxes walls
    | freeSpots == []   = ((rx,ry),boxes)
    | otherwise         = ((rx,ry-1),nextBoxes)
        where
            checkWalls = filter (\(wx,wy) -> (rx==wx) && (ry>wy)) walls
            wy1        = snd $ maximum checkWalls
            checkBoxes = filter (\(bx,by) -> (rx==bx) && (ry>by) && (by>wy1)) boxes
            freeSpots  = [ (rx,y) | y <- [ry-1,ry-2..wy1+1], not (elem (rx,y) checkBoxes) ]
            firstFree  = head freeSpots
            nextBoxes  = [firstFree] ++ filter (/=(rx,ry-1)) boxes 

moveBoxAndRobotLeft :: Location -> Locations -> Locations -> (Location,Locations)
moveBoxAndRobotLeft (rx,ry) boxes walls
    | freeSpots == []   = ((rx,ry),boxes)
    | otherwise         = ((rx-1,ry),nextBoxes)
        where
            checkWalls = filter (\(wx,wy) -> (ry==wy) && (rx>wx)) walls
            wx1        = fst $ maximum checkWalls
            checkBoxes = filter (\(bx,by) -> (ry==by) && (rx>bx) && (bx>wx1)) boxes
            freeSpots  = [ (x,ry) | x <- [rx-1,rx-2..wx1+1], not (elem (x,ry) checkBoxes) ]
            firstFree  = head freeSpots
            nextBoxes  = [firstFree] ++ filter (/=(rx-1,ry)) boxes 

moveBoxAndRobotRight :: Location -> Locations -> Locations -> (Location,Locations)
moveBoxAndRobotRight (rx,ry) boxes walls
    | freeSpots == []   = ((rx,ry),boxes)
    | otherwise         = ((rx+1,ry),nextBoxes)
        where
            checkWalls = filter (\(wx,wy) -> (ry==wy) && (rx<wx)) walls
            wx1        = fst $ minimum checkWalls
            checkBoxes = filter (\(bx,by) -> (ry==by) && (rx<bx) && (bx<wx1)) boxes
            freeSpots  = [ (x,ry) | x <- [rx+1..wx1-1], not (elem (x,ry) checkBoxes) ]
            firstFree  = head freeSpots
            nextBoxes  = [firstFree] ++ filter (/=(rx+1,ry)) boxes 

moveBoxAndRobotDown :: Location -> Locations -> Locations -> (Location,Locations)
moveBoxAndRobotDown (rx,ry) boxes walls 
    | freeSpots == []   = ((rx,ry),boxes)
    | otherwise         = ((rx,ry+1),nextBoxes)
        where
            checkWalls = filter (\(wx,wy) -> (rx==wx) && (ry<wy)) walls
            wy1        = snd $ minimum checkWalls
            checkBoxes = filter (\(bx,by) -> (rx==bx) && (ry<by) && (by<wy1)) boxes
            freeSpots  = [ (rx,y) | y <- [ry+1..wy1-1], not (elem (rx,y) checkBoxes) ]
            firstFree  = head freeSpots
            nextBoxes  = [firstFree] ++ filter (/=(rx,ry+1)) boxes 

sumBoxesGPS :: Locations -> Int
sumBoxesGPS = sum . map boxGPS
    where
    --  boxGPS :: Location -> Int
        boxGPS (x,y) = x+100*y

partOne :: Warehouse -> Int
partOne lines = sumBoxesGPS movedBoxes
    where
        (((robot:_):boxes:walls:_),moves) = parse lines
        (movedRobot,movedBoxes) = moveRobot robot boxes walls moves

---------------------------------------------------------------------------------------
-- Pretty print the Robot, Boxes, Walls and free space (part one)
pretty :: (Location,Locations,Locations) -> IO ()
pretty (robot,boxes,walls) = printSList plot
    where
        (mx,my) = maximum walls
        plot = [[ item (x,y) robot boxes walls | x <- [0..mx]] | y <- [0..my] ]
            where
                item xy rxy boxes walls 
                    | xy == rxy     = cRobot 
                    | elem xy boxes = cBox
                    | elem xy walls = cWall
                    | otherwise     = cFree

-- Print String list -> line by line
printSList :: [String] -> IO ()
printSList []     = do  return ()
printSList (l:ls) = do  putStrLn l
                        printSList ls


-- Part two ---------------------------------------------------------------------------
--
reshape :: [String] -> [String]
reshape = map (concat . map reshapeLocation) 

reshapeLocation :: Char -> String
reshapeLocation lc
    |   lc == cBox      = sBox
    |   lc == cRobot    = sRobot
    |   lc == cWall     = sWall
    |   lc == cFree
    ||  lc == cSpace    = sFree
    |   lc == cUp
    ||  lc == cRight
    ||  lc == cDown
    ||  lc == cLeft     = [lc]  -- movement codes
    | otherwise         = []    -- skip rest

parseReshaped :: [String] -> ([Locations],Directions)
parseReshaped lines = (whls,moves)
    where
        warehouseLines  = takeWhile (/="") lines
        directionsLines = concat $ dropWhile (/="") lines
        wxy             = warehouseSize warehouseLines
        whls            = parseReshapedWarehouse warehouseLines (0,0) wxy [] [] [] []
        moves           = parseMoves directionsLines

parseReshapedWarehouse :: Warehouse -> Location -> Location -> 
                            Locations -> Locations -> Locations -> Locations -> [Locations]
parseReshapedWarehouse whs (x,y) (wx,wy) rls blls blrs wls
    | (y>=wy)                   = [rls,blls,blrs,wls]
    | (x>=wx)                   = parseReshapedWarehouse whs (0,y+1) (wx,wy) rls blls blrs wls
    | whs!!y!!x == cBoxLeft     = parseReshapedWarehouse whs (x+1,y) (wx,wy) rls (blls++[(x,y)]) blrs wls
    | whs!!y!!x == cBoxRight    = parseReshapedWarehouse whs (x+1,y) (wx,wy) rls blls (blrs++[(x,y)]) wls
    | whs!!y!!x == cRobot       = parseReshapedWarehouse whs (x+1,y) (wx,wy) (rls++[(x,y)]) blls blrs wls
    | whs!!y!!x == cWall        = parseReshapedWarehouse whs (x+1,y) (wx,wy) rls blls blrs (wls++[(x,y)])
    | otherwise                 = parseReshapedWarehouse whs (x+1,y) (wx,wy) rls blls blrs wls


---- move robot -----------------------------------------------------------------------------------
--
moveRobotOnReshaped :: Location -> Locations -> Locations -> Locations -> Directions -> 
                        (Location,Locations,Locations,Locations)
moveRobotOnReshaped rxy boxesLeft boxesRight walls  []          = (rxy,boxesLeft,boxesRight,walls)
moveRobotOnReshaped rxy boxesLeft boxesRight walls (mxy:rms)
    | hitWall   = moveRobotOnReshaped  rxy boxesLeft boxesRight walls rms 
    | hitBox    = moveRobotOnReshaped brxy nBxsLeft  nBxsRight  walls rms
    | otherwise = moveRobotOnReshaped nrxy boxesLeft boxesRight walls rms
        where
            nrxy        = addPair rxy mxy
            hitWall     = elem nrxy walls
            hitBox      = (elem nrxy boxesLeft) || (elem nrxy boxesRight)
            (brxy,nBxsLeft,nBxsRight) = moveBoxesOnReshape rxy boxesLeft boxesRight walls mxy

-- Robot hits box (left or right), can box or boxes be moved? And if yes, just do so.
moveBoxesOnReshape :: Location -> Locations -> Locations -> Locations -> Direction -> 
                                (Location,Locations,Locations)
moveBoxesOnReshape rxy boxesLeft boxesRight walls mxy 
    | mxy == dUp   || mxy == dDown  = moveBoxesOnReshapeUpDown    rxy mxy boxesLeft boxesRight walls
    | mxy == dLeft || mxy == dRight = moveBoxesOnReshapeLeftRight rxy mxy boxesLeft boxesRight walls
    | otherwise                     = error " * Missing or illegal direction! * "

-- Horizontal moves ---------------------------------------------------------------------------------
--
nextToBoxesInLine :: Location -> Direction -> Locations -> Locations -> Locations -> Locations
nextToBoxesInLine cxy mxy boxes walls moveBoxes
    | isBoxPart   = nextToBoxesInLine txy mxy boxes walls (txy:moveBoxes)
    | isFreeSpot  = moveBoxes
    | otherwise   = []
        where
            txy         = addPair cxy mxy
            isBoxPart   = elem txy boxes
            isFreeSpot  = not (elem txy walls)

moveBoxesOnReshapeLeftRight :: Location -> Direction -> Locations -> Locations -> Locations ->
                                (Location,Locations,Locations) 
moveBoxesOnReshapeLeftRight rxy mxy boxesLeft boxesRight walls
    | moveBoxes == []   = (rxy,boxesLeft,boxesRight)
    | otherwise         = (nextRxy,nextBoxesLeft,nextBoxesRight)
        where
            moveBoxes   = nextToBoxesInLine rxy mxy (boxesLeft ++ boxesRight) walls []
            nextRxy     = addPair rxy mxy 
            fixedBoxesLeft  = filter (\blxy -> not (elem blxy moveBoxes)) boxesLeft
            fixedBoxesRight = filter (\brxy -> not (elem brxy moveBoxes)) boxesRight
            moveBoxesLeft   = map (addPair mxy) $ filter (\blxy -> elem blxy moveBoxes) boxesLeft
            moveBoxesRight  = map (addPair mxy) $ filter (\brxy -> elem brxy moveBoxes) boxesRight
            nextBoxesLeft   = moveBoxesLeft  ++ fixedBoxesLeft
            nextBoxesRight  = moveBoxesRight ++ fixedBoxesRight


-- Vertical moves -------------------------------------------------------------------------------------
--
collectHitBoxes :: Direction -> Boxes -> Boxes -> Boxes -> Boxes
collectHitBoxes _   collection []           _       = collection
collectHitBoxes mxy collection testRowBoxes boxes   =
    collectHitBoxes mxy newCollection nextTestBoxes boxes
        where
            borderInLine    = [ borderingBox |  testBox <- testRowBoxes, 
                                                let borderingBox = addPairPair mxy testBox, 
                                                elem borderingBox boxes ]
            borderAtLeft    = [ borderingBox |  testBox <- testRowBoxes, 
                                                let borderingBox = addPairPair (addPair mxy dLeft) testBox,
                                                elem borderingBox boxes ]
            borderAtRight   = [ borderingBox |  testBox <- testRowBoxes, 
                                                let borderingBox = addPairPair (addPair mxy dRight) testBox,
                                                elem borderingBox boxes ]
                            -- 'nub' -> only collect unique boxes 
            nextTestBoxes   = nub $ borderInLine ++ borderAtLeft ++ borderAtRight
            newCollection   = collection ++ nextTestBoxes

collisionCheck :: Direction -> Locations -> Locations -> Bool
collisionCheck mxy movedBoxes walls = 
    [ movedBox | movedBox <- movedBoxes, elem movedBox walls ] /= []

moveBoxesOnReshapeUpDown :: Location -> Direction -> Locations -> Locations -> Locations ->
                                (Location,Locations,Locations) 
moveBoxesOnReshapeUpDown rxy mxy boxesLeft boxesRight walls
    | blockedBoxes  = (rxy,boxesLeft,boxesRight)
    | otherwise     = (nextRxy,nextBoxesLeft,nextBoxesRight)
        where
            nextRxy         = addPair rxy mxy 
            nextRxyR        = addPair nextRxy dLeft
            getHitBoxLeft   = filter (\bxy -> (bxy == nextRxy || bxy == nextRxyR)) boxesLeft
            hitBoxLeft      = head getHitBoxLeft    -- should found one, HIT test is done in 'moveRobotOnReshaped'
            hitBoxRight     = addPair hitBoxLeft dRight
            hitBox          = (hitBoxLeft,hitBoxRight)
            boxes           = map (\blxy -> (blxy,addPair blxy dRight)) boxesLeft
            hitBoxes        = collectHitBoxes mxy [hitBox] [hitBox] boxes
            hitBoxesLeft    = map fst hitBoxes
            hitBoxesRight   = map snd hitBoxes
            movedBoxesLeft  = map (addPair mxy) hitBoxesLeft
            movedBoxesRight = map (addPair mxy) hitBoxesRight
            blockedBoxes    = collisionCheck mxy (movedBoxesLeft ++ movedBoxesRight) walls
            fixedBoxesLeft  = filter (\blxy -> not (elem blxy hitBoxesLeft)) boxesLeft
            fixedBoxesRight = filter (\brxy -> not (elem brxy hitBoxesRight)) boxesRight
            nextBoxesLeft   = movedBoxesLeft  ++ fixedBoxesLeft
            nextBoxesRight  = movedBoxesRight ++ fixedBoxesRight

partTwoAllData :: Warehouse -> (Location,Locations,Locations,Locations)
partTwoAllData lines = moveRobotOnReshaped robot boxesLeft boxesRight walls moves
    where
        (((robot:_):boxesLeft:boxesRight:walls:_),moves)    = parseReshaped $ reshape lines

partTwo :: Warehouse -> Int
partTwo = sumBoxesGPS . getBoxesLeft . partTwoAllData
    where
        getBoxesLeft (_,bls,_,_) = bls
  ------------------------------------------------------------------------------------------------
-- Pretty print the Robot, Boxes, Walls and free space (part two)
-- 
partTwoPretty :: Warehouse -> IO ()
partTwoPretty =  prettyReshaped . partTwoAllData

prettyReshaped :: (Location,Locations,Locations,Locations) -> IO ()
prettyReshaped (robot,boxesLeft,boxesRight,walls) = printSList plot
    where
        (mx,my) = maximum walls
        plot = [[ item (x,y) robot boxesLeft boxesRight walls | x <- [0..mx]] | y <- [0..my] ]
            where
                item xy rxy boxesLeft boxesRight walls 
                    | xy == rxy             = cRobot 
                    | elem xy boxesLeft     = cBoxLeft
                    | elem xy boxesRight    = cBoxRight
                    | elem xy walls         = cWall
                    | otherwise             = cFree


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 15 (Haskell)"
            day15 <- lines <$> readFile filename
            putStr "Part one: the sum of all boxes' GPS coordinates is:       "
            print $ partOne day15
            putStr "Part two: the sum of all boxes' final GPS coordinates is: "
            print $ partTwo day15
            -- Optional pretty plot
            -- partTwoPretty day15
            putStrLn "0K.\n"

--  End of code
