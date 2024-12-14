-- Advent of Code 2024 - Day 14 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: safety factor after 100 seconds is:        218433348
--  Part two: number of seconds for first easter egg is:      6512
--
-- (cl) by Arno Jacobs, 2024-12-14

-- 
module AoC2024d14ab where

import Data.Char (isDigit)

type Location   = (Int,Int)             -- tile location, with (0,0) is top left
type Locations  = [Location]
type Speed      = (Int,Int)             -- tiles / second
type Robot      = (Location,Speed)
type Robots     = [Robot]


-- Some initials
filename :: String
filename = "data/inputDay14_2024.txt"

seconds :: Int
seconds = 100

-- bathroom size
bathroom :: Location
bathroom = (101,103)

-- half ways bathroom - to centre spot
centerBathroom :: Location
centerBathroom = (fst bathroom `div` 2, snd bathroom `div` 2)

-- Parse input data ------------------------------------------------------------
--
parse :: String -> Robot
parse rs    | length xs > 3 = ((xs!!0,xs!!1),(xs!!2,xs!!3))
            | otherwise     = ((0,0),(0,0))
    where 
        xs = getIntsList rs

getIntsList :: String -> [Int]
getIntsList []      = []
getIntsList (x:xs)  | ipN /= [] && x == '-' = [- read ipN] ++ getIntsList rds
                    | ipP /= []             = [  read ipP] ++ getIntsList rds
                    | otherwise             = getIntsList xs
    where
        ipN     = takeWhile isDigit xs
        ipP     = takeWhile isDigit (x:xs)
        rds     = dropWhile isDigit xs
        

-- Part one --------------------------------------------------------------------
--
walkies :: Int -> Robot -> Robot
walkies s ((px,py),(vx,vy)) =
    ((( px + s * vx) `mod` fst bathroom,
      ( py + s * vy) `mod` snd bathroom ), (vx,vy)) 

quadrantCounts :: Robots -> [Int]
quadrantCounts robots = 
    [   sum [ count (x,y) | x <- [   0..hx-1], y <- [   0..hy-1]],  -- top left
        sum [ count (x,y) | x <- [hx+1..mx-1], y <- [   0..hy-1]],  -- top right
        sum [ count (x,y) | x <- [   0..hx-1], y <- [hy+1..my-1]],  -- bottom left
        sum [ count (x,y) | x <- [hx+1..mx-1], y <- [hy+1..my-1]]   -- bottom right    
     ]
        where
            rps         = map fst robots
            (hx,hy)     = centerBathroom
            (mx,my)     = bathroom
            count xy    = length $ filter (==xy) rps

partOne :: Robots -> Int
partOne = product . quadrantCounts . map (walkies seconds)


-- Part two --------------------------------------------------------------------
--
-- With a little help of my friends --------------------------------------------
-- Make sure all locations of the robots unique, 
-- NO two or more robots on one spot.
-- This will show the easter egg XMAS tree . . . was I lucky?
--
isImage :: Robots -> Bool
isImage robots = allUnique (map fst robots)

allUnique :: Locations -> Bool 
allUnique es = [ e | e <- es, length (filter (==e) es) == 1 ] == es

partTwo :: Robots -> Int
partTwo robots = head [ s | s <- [1..], isImage (map (walkies s) robots) ]   


-- Helper code for 'plotting' center part of bathroom --------------------------
-- 
plotEasterEgg :: Robots -> IO ()
plotEasterEgg robots = 
    printSList [[ plotChar r
                    | x <- [border..fst bathroom - border], 
                        let r = elem (x,y) robotPositions ]
                    | y <- [border..snd bathroom - border]]
        where
            border          = 28
            plotChar True   = '*'
            plotChar _      = '.'
            seconds         = partTwo robots
            robotPositions  = map (fst . walkies seconds) robots
            printSList []     = do  return ()
            printSList (l:ls) = do  putStrLn l
                                    printSList ls


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 14 (Haskell)"
            day14 <- map parse <$> lines <$> readFile filename
            putStr "Part one: safety factor after 100 seconds is:        "
            print $ partOne day14
            putStr "Part two: number of seconds for first easter egg is:      "
            print $ partTwo day14
            -- Optional plot of the easter egg
            -- plotEasterEgg day14
            putStrLn "0K.\n"

--  End of code
