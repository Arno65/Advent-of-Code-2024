-- Advent of Code 2024 - Day 5 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: Sum of middle page numbers from correctly-ordered updates: 5248
-- Part two: Sum of middle page numbers from corrected-ordered updates: 4507
--
-- (cl) by Arno Jacobs, 2024-12-05

module AoC2024d05ab where

import Data.List.Split (splitOn)

-- Some initials
filename :: String
filename = "data/inputDay05_2024.txt"


-- parsing --------------------------------------------------------

pageOrderinfRules :: [String] -> [(Int,Int)]
pageOrderinfRules xs = map pair porPart
    where
        porPart = takeWhile (/="") xs  
        pair s  = (read (sp!!0), read (sp!!1))
            where
                sp = splitOn "|" s

-- indexed page numbers to produce                
pagesToProduce :: [String] -> [[(Int,Int)]]
pagesToProduce xs = map indexPages p2psPart
    where
        p2psPart        = tail $ dropWhile (/="") xs
        linePages       = map read . splitOn ","
        indexPages ls   = zip [0..] (linePages ls)

-- Part one -------------------------------------------------------

checkOrders :: Bool -> [(Int,Int)] -> [[(Int,Int)]] -> [[(Int,Int)]]
checkOrders check pors = filter (\p2p -> check == checkOrder pors p2p) 

checkOrder :: [(Int,Int)] -> [(Int,Int)] -> Bool
checkOrder []         _     = True
checkOrder (por:pors) p2ps  | rightOrder por p2ps   = checkOrder pors p2ps
                            | otherwise             = False

rightOrder :: (Int,Int) -> [(Int,Int)] -> Bool
rightOrder (ps,pe) p2p  | pis == [] || pie == []    = True
                        | si < ei                   = True
                        | otherwise                 = False
    where
        pis = filter (\(_,p) -> p == ps ) p2p 
        pie = filter (\(_,p) -> p == pe ) p2p 
        si  = fst $ head pis
        ei  = fst $ head pie

partOne :: [(Int,Int)] -> [[(Int,Int)]] -> Int
partOne prs pps = sum (map snd mns)
    where
        cps = checkOrders True prs pps
        mns = map middle cps
            where
            -- NO range or empty list checking
            --  middle :: [a] -> a
                middle xs = xs !! (div (length xs) 2)


-- Part two -------------------------------------------------------

repairOrders :: [(Int,Int)] -> [[(Int,Int)]] -> [[(Int,Int)]] 
repairOrders _     []           = []
repairOrders pors (p2p:rp2ps)   = [repairedP2p] ++ repairOrders pors rp2ps
    where
        repairedP2p = repairOrder pors p2p

repairOrder :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
repairOrder []          p2p = p2p
repairOrder (por:rpors) p2p = repairOrder rpors repairedP2p        
    where
        repairedP2p = repairSingleOrderRule por p2p

repairSingleOrderRule :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
repairSingleOrderRule (ps,pe) p2p   | pis == [] || pie == []    = p2p
                                    | si < ei                   = p2p
                                    | otherwise                 = repairedOrder
    where
        pis = filter (\(_,p) -> p == ps ) p2p 
        pie = filter (\(_,p) -> p == pe ) p2p 
        si  = fst $ head pis
        ei  = fst $ head pie
        repairedOrder = repairSingleOrder (si,ei) p2p

-- Place 'end' before 'start' index
repairSingleOrder :: (Int,Int) ->  [(Int,Int)] -> [(Int,Int)]
repairSingleOrder (si,ei) p2p = zip [0..] $ map snd ps   -- new index
    where
        pm = take (si-ei-1) $ drop (ei+1) p2p       -- Ugly but very 0K.
        ps = take ei p2p ++ [p2p !! si , p2p !! ei] ++ pm ++ drop (si+1) p2p

cummulateRepairedOrders :: [(Int,Int)] -> [[(Int,Int)]] -> [[(Int,Int)]]
cummulateRepairedOrders _   []      = [] 
cummulateRepairedOrders prs rpps    = 
    correctedOrders ++ cummulateRepairedOrders prs incorrectOrders
        where
            ros             = repairOrders prs rpps
            correctedOrders = checkOrders True  prs ros
            incorrectOrders = checkOrders False prs ros

partTwo :: [(Int,Int)] -> [[(Int,Int)]] -> Int
partTwo prs pps = sum (map snd mns)
    where
        incorrectOrders = checkOrders False prs pps
        correctedOrders = cummulateRepairedOrders prs incorrectOrders
        mns = map middle correctedOrders
            where
            -- NO range or empty list checking
            --  middle :: [a] -> a
                middle xs = xs !! (div (length xs) 2)

main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 5  (Haskell)"
            day5 <- lines <$> readFile filename
            let pors = pageOrderinfRules day5
            let p2ps = pagesToProduce day5
            putStr "Part one: sum of middle page numbers (correctly-ordered updates): "
            print $ partOne pors p2ps
            putStr "Part two: sum of middle page numbers (corrected-ordered updates): "
            print $ partTwo pors p2ps 

            putStrLn "0K.\n"

--  End of code
