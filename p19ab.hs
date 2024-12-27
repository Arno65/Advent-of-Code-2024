-- Advent of Code 2024 - Day 19 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: the number of possible designs:  209
-- Part two: the number of different designs: 777669668613191
--
-- (cl) by Arno Jacobs, 2024-12-27

-- module AoC2024d19ab where

import Data.List.Split (splitOn)

-- Some initials
filename :: String
filename = "data/inputDay19_2024.txt"

-- parse helper ------------------------------------------------------------
-- 
patterns :: String -> [String]
patterns = map stripSpaces . splitOn ","

stripSpaces :: String -> String
stripSpaces []      = []
stripSpaces (x:xs)  | x == ' '  = stripSpaces xs
                    | otherwise = (x:xs)

designs :: [String] -> [String]
designs = dropWhile (== "")

parse :: [String] -> ([String],[String])
parse (line:lines) = (patterns line,designs lines)

-- Part one  ------------------------------------------------------------
-- 

-- This is not really needed -- a little speed increaser
inString :: String -> String -> Bool
inString _  []  = False 
inString ls sp  | lnsp > length ls  = False
                | lsp == sp         = True
                | otherwise         = inString (tail ls) sp
    where
        lnsp = length sp
        lsp  = take lnsp ls

testDesign :: [String] -> String -> Bool
testDesign ps d = testDesign' mps d
    where 
        mps = filter (inString d) ps    -- Only use patterns in design

testDesign' :: [String] -> String -> Bool
testDesign' mps d   
    | endMatches  /= [] = True
    | headMatches == [] = False
    | otherwise         = or [ testDesign' nps nd | (nps,nd) <- headMatches ] 
        where
            endMatches  = [ 1 | mp <- mps, mp == d ]
            headMatches = [ (mps,drop (length mp) d) | 
                                mp <- mps, 
                                atStart mp d,
                                let rmp = filter (/=mp) mps ]
                where 
                --  atStart :: String -> String -> Bool
                    atStart ts s = ts == take (length ts) s

partOne :: ([String],[String]) -> Int
partOne (ps,ds) = length $ filter (testDesign ps) ds

-- Part two  ------------------------------------------------------------
-- 
countDesigns :: [String] -> String -> Int
countDesigns ps design = countDesigns' ps [] design (length design - 1)

countDesigns' :: [String] -> [(String,Int)] -> String -> Int -> Int
countDesigns' ps cps design 0   = snd $ head $ getCountPart ps cps design
countDesigns' ps cps design cc  = countDesigns' ps nextCps design (cc-1)
    where
        nextCps = getCountPart ps cps $ drop cc design

getCountPart :: [String] -> [(String,Int)] -> String -> [(String,Int)] 
getCountPart ps cps dp = insert (dp,nc) cps
    where
        nc = sum [ getCount cps (drop pln dp) |   
                    p <- ps,
                    let pln = length p,
                    p == take pln dp ]

insert :: (String,Int) -> [(String,Int)] -> [(String,Int)]
insert (dp,nc) cps = ((dp,nc):ncps)
    where
        ncps = filter (\(p,_) -> p/=dp) cps

getCount :: [(String,Int)] -> String -> Int
getCount _   []     = 1
getCount cps dpp    | cls == [] = 0
                    | otherwise = (snd . head) cls
    where 
        cls = filter (\(p,c) -> p == dpp) cps

partTwo :: ([String],[String]) -> Int
partTwo (ps,ds) = sum $ map (\d -> countDesigns ps d) (filter (testDesign ps) ds)


main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 19  (Haskell)"
            day19 <- parse <$> lines <$> readFile filename
            putStr "Part one: the number of possible designs:  "
            print $ partOne day19
            putStr "Part two: the number of different designs: "
            print $ partTwo day19
            putStrLn "0K.\n"


-- End of code
