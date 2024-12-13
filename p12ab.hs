-- Advent of Code 2024 - Day 12 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: Total price of fencing all regions:     1465112
--  Part two: Total new price of fencing all regions:  893790
--
-- * I needed some extra time for clean coding this very challenging assignment.
--
-- (cl) by Arno Jacobs, 2024-12-13

-- module AoC2024d12ab where

import Data.List (nub)

type Plant          = Char
type Farm           = [String] 
type Location       = (Int,Int)
type Direction      = (Int,Int)
type Locations      = [Location]
type Directions     = [Direction]
type PlantGroups    = [Locations]

-- Some initials
filename :: String
filename = "data/inputDay12_2024.txt"

east   = ( 1, 0) :: Direction
south  = ( 0, 1) :: Direction
west   = (-1, 0) :: Direction
north  = ( 0,-1) :: Direction

-- Corner test - both NOT plant  -  or
-- Corner test - two sides are plant, diagonal is NOT
cornerDirections :: [Directions]
cornerDirections = [northWest,northEast,southEast,southWest]
    where
        northWest   = [north,west]
        northEast   = [north,east]
        southWest   = [south,west]
        southEast   = [south,east]

addPair :: Direction -> Direction -> Direction
addPair (x1,y1) (x2,y2) = (x1+x2,y1+y2)

farmSize :: Farm -> Location
farmSize ps = (length (head ps), length ps)

-- Range checking
onFarm :: Location -> Location -> Locations
onFarm (px,py) (mx,my)  | px >= 0 && px < mx && py >= 0 && py < my  = [(px,py)]
                        | otherwise                                 = []

-- Retreive plant
getPlant :: Farm -> Location -> Plant
getPlant farm (px,py) = farm!!py!!px

-- Test if the plant is on given location
isPlant :: Farm -> Location -> Plant -> Bool    
isPlant farm (px,py) plant = farm!!py!!px == plant

----------------------------------------------------------------------------------------------
-- Parsing and Flood fill the farm area with the different plants
--
groupPlants :: Farm -> PlantGroups
groupPlants farm = groupPlants' farm (0,0) (farmSize farm) []
        
groupPlants' :: Farm -> Location -> Location -> Locations -> PlantGroups
groupPlants' farm (px,py) (mx,my) markedLocations
    | px >= mx                      = groupPlants' farm (0,py+1) (mx,my) markedLocations
    | py >= my                      = []
    | elem (px,py) markedLocations  = groupPlants' farm (px+1,py) (mx,my) markedLocations
    | otherwise                     = newPlantsGroup ++ groupPlants' farm (px+1,py) (mx,my) newMarked
        where 
            plant           = getPlant farm (px,py)
            plantsList      = groupSinglePlants farm [(px,py)] (mx,my) plant [(px,py)]
            newPlantsGroup  = [plantsList]
            newMarked       = markedLocations ++ plantsList      

groupSinglePlants :: Farm -> Locations -> Location -> Plant -> Locations -> Locations
groupSinglePlants farm pxys mxy plant planted
    | nextPlants == []  = planted
    | otherwise         = groupSinglePlants farm nextPlants mxy plant (nextPlants ++ planted)
    where 
        nextPlants = (nub . concat) [ borderingPlants farm pxy mxy plant planted | pxy <- pxys ]

borderingPlants :: Farm -> Location -> Location -> Plant -> Locations -> Locations
borderingPlants farm pxy mxy plant planted = 
    [ nxy | nxy <- testLocations,       
            getPlant farm nxy == plant, -- Is it the plant we are looking for?
            not (elem nxy planted) ]    -- Has it been found yet?
    where
        testLocations   =   onFarm (addPair pxy east)  mxy 
                        ++  onFarm (addPair pxy south) mxy 
                        ++  onFarm (addPair pxy west)  mxy 
                        ++  onFarm (addPair pxy north) mxy 

----------------------------------------------------------------------------------------------
-- Fencing part one
--
fencePlacementOne :: Locations -> Int
fencePlacementOne plants = sum [ fenceCount plant plants | plant <- plants ]
    where
        fenceCount pxy plants = sum [ 1 | bl <- borderLocations, not (elem bl plants) ]
            where 
                borderLocations = [ addPair pxy east, addPair pxy south, 
                                    addPair pxy west, addPair pxy north ]

----------------------------------------------------------------------------------------------
-- Fencing part two
--
fencePlacementTwo :: Locations -> Int
fencePlacementTwo pxys = sum [ countCorners cornerDirections pxy pxys | pxy <- pxys ]

countCorners :: [Directions] -> Location -> Locations -> Int
countCorners []       _   _     = 0
countCorners (wd:wds) pxy pxys  = cco + cci + countCorners wds pxy pxys
    where
        dxy1 = head wd
        dxy2 = last wd
        -- 'inside' corner
        ib1 = elem (addPair pxy dxy1) pxys
        ib2 = elem (addPair pxy dxy2) pxys
        ib3 = not (elem (addPair pxy (addPair dxy1 dxy2)) pxys)
        cci = if and [ib1,ib2,ib3] then 1 else 0
        -- 'outside' corner
        cco = if not (ib1 || ib2) then 1 else 0


costCalculation :: Int -> Farm -> Int
costCalculation pricing farm = sum [ g*f | (g,f) <- zip gpc fpc ]
    where
        gps = groupPlants farm
        gpc = map length gps
        fpc = if (pricing==1)   then map fencePlacementOne gps
                                else map fencePlacementTwo gps


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 12 (Haskell)"
            day12 <- lines <$> readFile filename
            putStr "Part one: Total price of fencing all regions:     "
            print $ costCalculation 1 day12
            putStr "Part two: Total new price of fencing all regions:  "
            print $ costCalculation 2 day12
            putStrLn "0K.\n"

--  End of code
