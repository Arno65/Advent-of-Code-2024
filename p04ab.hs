-- Advent of Code 2024 - Day 4 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: XMAS  count: 2390
-- Part two: X-MAS count: 1809
--
-- (cl) by Arno Jacobs, 2024-12-04

module AoC2024d04ab where

data X a = NoX | OutOfRange | Count a 
            deriving (Show,Eq)

-- Some initials
filename :: String
filename = "data/inputDay04_2024.txt"

-- Part one test result: 18
-- Part two test result:  9

sXMAS   = "XMAS" :: String
sX_WORD =  "MAS" :: String

-- Part one
-- Looking for "XMAS"
countWord :: [String] -> String -> Int
countWord letterGrid lookingFor = 
    sum [ countWord' letterGrid lookingFor (x,y) wln | x <- [0..maxX-1], y <- [0..maxY-1]]
        where
            maxY = length letterGrid
            maxX = length (head letterGrid)
            wln  = length lookingFor

countWord' :: [String] -> String -> (Int,Int) -> Int -> Int
countWord' letterGrid lookingFor pxy wln =
    sum [ checkWord letterGrid lookingFor pxy (dx,dy) wln | 
            dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0 ]

checkWord :: [String] -> String -> (Int,Int) -> (Int,Int) -> Int -> Int
checkWord letterGrid lookingFor pxy dxy wln 
    | lookingFor == getWord letterGrid pxy dxy wln  = 1
    | otherwise                                     = 0
        
-- First creating the words in all 8 directions first and then checking 
-- is not the quickest solution, but fine for here and now. 
getWord :: [String] ->  (Int,Int) -> (Int,Int) -> Int -> String
getWord letterGrid (px,py) (dx,dy) wln =
    [ getLetterSafe letterGrid (px+dx*ix,py+dy*ix) | ix <- [0..wln-1] ]

getLetterSafe :: [String] -> (Int,Int) -> Char        
getLetterSafe letterGrid (x,y)  
    | x < 0 || y < 0 || x >= maxX || y >= maxY  = '~'                   -- A dummy character
    | otherwise                                 = letterGrid !! y !! x
        where
            maxY = length letterGrid
            maxX = length (head letterGrid)

-- Part two
-- Looking for:  M   S          S   M        M   M         S   S
--                 A      or      A     or     A      or     A
--               M   S          S   M        S   S         M   M
-- Four rotations.

-- Generic algorithm for words with an odd number of letters
--
countXWord :: [String] -> String -> X Int
countXWord letterGrid lookingFor 
    | border >= minMax-border-1 = OutOfRange
    | even (length lookingFor)  = NoX
    | otherwise                 = Count $ sum [ countXWord' letterGrid lookingFor (cx,cy) border | 
                                                cx <- [border..maxX-border-1], 
                                                cy <- [border..maxY-border-1]]
        where
            maxY    = length letterGrid
            maxX    = length (head letterGrid)
            minMax  = min maxX maxY
            border  = (length lookingFor) `div` 2   -- also: arm length

countXWord' :: [String] -> String -> (Int,Int) -> Int -> Int
countXWord' letterGrid lookingFor cxy armLength
    | isX_MAS letterGrid lookingFor cxy armLength   = 1
    | otherwise                                     = 0

isX_MAS :: [String] -> String -> (Int,Int) -> Int -> Bool
isX_MAS letterGrid lookingFor cxy armLength =
        isX_MAS' letterGrid lookingFor lookingFor cxy armLength     -- initial rotation
    ||  isX_MAS' letterGrid lookingFor roFgnikool cxy armLength     -- rotation two
    ||  isX_MAS' letterGrid roFgnikool lookingFor cxy armLength     -- rotation three
    ||  isX_MAS' letterGrid roFgnikool roFgnikool cxy armLength     -- rotation four
            where
                roFgnikool = reverse lookingFor

isX_MAS' :: [String] -> String -> String -> (Int,Int) -> Int -> Bool
isX_MAS' letterGrid lookRight lookLeft (cx,cy) armLength = 
        lookRight == wordRight
    &&  lookLeft  == wordLeft
            where
                wordRight = [ getLetterSafe letterGrid (cx+d,cy+d) | d <- [-armLength..armLength]]
                wordLeft  = [ getLetterSafe letterGrid (cx+d,cy-d) | d <- [-armLength..armLength]]


prettyX :: X Int -> String
prettyX NoX         = "No correct X-word! Word length should be odd."
prettyX OutOfRange  = "X-word out of range!"
prettyX (Count c)   = show c

main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 4  (Haskell)"
            day4 <- lines <$> readFile filename
            putStr "Part one: XMAS  count: "
            print $ countWord  day4 sXMAS
            putStr "Part two: X-MAS count: "
            putStrLn $ prettyX $ countXWord day4 sX_WORD
            putStrLn "0K.\n"

--  End of code
