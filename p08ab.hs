-- Advent of Code 2024 - Day 8 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: The number of unique antinode locations is:  392
--  Part two: The number of unique antinode locations is: 1235
--
-- (cl) by Arno Jacobs, 2024-12-08

module AoC2024d08ab where

import Data.List (nub) -- 'nub' creates a set, a list with all unique elements

-- Some initials
filename :: String
filename = "data/inputDay08_2024.txt"


type Grid               = [String]
type Location           = (Int,Int)
type AntennaData        = (Char,Location)
type FrequencyLocations = (Char,[Location])
type Antinodes          = [Location]

cNoAntenna  = '.' :: Char

-- Assuming all rows are equal length
getGridSize :: [[a]] -> Location
getGridSize grid    = (length (head grid), length grid)

-- Part one ------- ------- ------- ------- ------- ------- ------- ------- ------- ------- -------

cummulateAntennaData :: Grid -> [AntennaData]
cummulateAntennaData roof = 
    [ (a,(x,y)) | x <- [0..mx-1], y <- [0..my-1], let a = roof !! y !! x, a /= cNoAntenna ]
        where
            (mx,my) = getGridSize roof

sweepPerFrequency :: [AntennaData] -> [FrequencyLocations]
sweepPerFrequency []            = []
sweepPerFrequency ((f,p):fps)   = [(f,ps)] ++ sweepPerFrequency rfps
    where
        ps      = [p] ++ map snd (filter (\(c,_) -> f == c) fps)
        rfps    = filter (\(c,_) -> f /= c) fps

calculateAntinodes :: ( Location -> Location -> Location -> Antinodes) ->
                            Location -> FrequencyLocations -> Antinodes
calculateAntinodes anFn mxy (f,ps) = 
    concat [ anFn mxy (ps !! i1) (ps !! i2) | 
                i1 <- [0..length ps-2], i2 <- [i1+1..length ps-1]]
                
pairAntinodes :: Location -> Location -> Location -> Antinodes
pairAntinodes mxy (x1,y1) (x2,y2)   | y1 > y2   = pairAntinodes mxy (x2,y2) (x1,y1)
                                    | otherwise = antinode1s ++ antinode2s
    where
        (deltax,deltay) = (x2-x1,y2-y1)
        antinode1s = resonantAntinodes mxy (deltax,deltay) (x1,y1) [-1]
        antinode2s = resonantAntinodes mxy (deltax,deltay) (x2,y2) [ 1]

resonantAntinodes :: Location -> Location -> Location -> [Int] -> Antinodes
resonantAntinodes mxy (dx,dy) (x,y) steps = 
    takeWhile (antennaOnRoof mxy) [ axy  | s <- steps, let axy = (x+s*dx,y+s*dy) ]
        where
        --  antennaOnRoof :: Location -> Location -> Bool
            antennaOnRoof (mx,my) (ax,ay) = (ax>=0) && (ax<mx) && (ay>=0) && (ay<my)

countAntinodes :: (Location -> Location -> Location -> Antinodes) -> 
                    Location -> [FrequencyLocations] -> Int
countAntinodes anFn mxy = length . nub . concat . map (calculateAntinodes anFn mxy)

partOne :: Grid -> Int
partOne roof = countAntinodes pairAntinodes (getGridSize roof) sds
    where
        sds = (sweepPerFrequency . cummulateAntennaData) roof


-- Part two ------- ------- ------- ------- ------- ------- ------- ------- ------- ------- -------

allAntinodes :: Location -> Location -> Location -> Antinodes
allAntinodes mxy (x1,y1) (x2,y2)    | y1 > y2   = allAntinodes mxy (x2,y2) (x1,y1)
                                    | otherwise = antinodes1 ++ antinodes2
    where
        (deltax,deltay) = (x2-x1,y2-y1)
        antinodes1      = resonantAntinodes mxy (-deltax,-deltay) (x1,y1) [0..] 
        antinodes2      = resonantAntinodes mxy ( deltax, deltay) (x2,y2) [0..]

partTwo :: Grid -> Int
partTwo roof = countAntinodes allAntinodes (getGridSize roof) sds
    where
        sds = (sweepPerFrequency . cummulateAntennaData) roof


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 8  (Haskell)"
            day8 <- lines <$> readFile filename
            putStr "Part one: The number of unique antinode locations is:  "
            print $ partOne day8
            putStr "Part two: The number of unique antinode locations is: "
            print $ partTwo day8
            putStrLn "0K.\n"

--  End of code

