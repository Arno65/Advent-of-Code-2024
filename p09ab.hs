-- Advent of Code 2024 - Day 9 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
--  Part one: Resulting filesystem checksum: 6471961544878
--  Part two: Resulting filesystem checksum: 6511178035564
--
-- (cl) by Arno Jacobs, 2024-12-09

-- Compiled ~ 6 seconds runtime on my M1
-- module AoC2024d09ab where

-- Some initials
filename :: String
filename = "data/inputDay09_2024.txt"

freeSpace :: Int
freeSpace = -1

noFreeBlocks :: Int
noFreeBlocks = -99

individualBlock :: Char -> Int -> [(Int,Int)]
individualBlock '0' _   = []
individualBlock  c  n   = [(read [c], n)] 

parseDiskMap :: String -> [(Int,Int)]
parseDiskMap = parseDiskMap' 0
    where
    --  parseDiskMap' :: Int -> String -> [(Int,Int)]
        parseDiskMap' idn []            = []                      
        parseDiskMap' idn (fls:[])      = [(read [fls],idn)]     
        parseDiskMap' idn (fls:_:[])    = [(read [fls],idn)]     
        parseDiskMap' idn (fls:fr:rdm)  =
                individualBlock fls idn 
            ++  individualBlock fr  freeSpace
            ++  parseDiskMap' (idn+1) rdm

idList :: [(Int,Int)] -> [Int]
idList = concat . map idList'
    where
        idList' (fls,idn)   
            | idn > freeSpace   = replicate fls idn
            | otherwise         = replicate fls 0


checkSum :: [Int] -> Int
checkSum = sum . map (\(f,id) -> f*id) . zip [0..] 

-- Part one ---------------------------------------------------------------------------------

moveFileBlocks ::  [(Int,Int)] ->  [(Int,Int)]
moveFileBlocks blocks   | lastIdn < 0           = moveFileBlocks (init blocks)
                        | firstFree == []       = blocks
                        | freeCnt >  lastFls    = moveFileBlocks nextBlocks1
                        | freeCnt == lastFls    = moveFileBlocks nextBlocks2
                        | otherwise             = moveFileBlocks nextBlocks3
    where
        workedBlocks        = takeWhile (\(_,idn) -> idn > freeSpace) blocks
        firstFree           = dropWhile (\(_,idn) -> idn > freeSpace) blocks
        (freeCnt,_)         = head firstFree
        restBlocks          = tail firstFree
        (lastFls,lastIdn)   = last blocks
        nextBlocks1         =   workedBlocks 
                            ++  [(lastFls,lastIdn)] 
                            ++  [(freeCnt-lastFls,freeSpace)] 
                            ++  init restBlocks
        nextBlocks2         =   workedBlocks 
                            ++  [(lastFls,lastIdn)] 
                            ++  init restBlocks
        nextBlocks3         =   workedBlocks 
                            ++  [(freeCnt,lastIdn)] 
                            ++  init restBlocks 
                            ++  [(lastFls-freeCnt,lastIdn)] 
            

-- Part two ---------------------------------------------------------------------------------

defrag :: [(Int,Int)] ->  [(Int,Int)]
defrag blocks = defrag' (length blocks - 1) blocks

defrag' :: Int ->  [(Int,Int)] -> [(Int,Int)] 
defrag' 0  blocks   = blocks
defrag' ix blocks   | idn == freeSpace      = defrag' (ix-1) blocks
                    | frIx == noFreeBlocks  = defrag' (ix-1) blocks
                    | fls == fr             = defrag' (ix-1) newBlocks1
                    | otherwise             = defrag'  ix    newBlocks2 -- fls < fr
    where
        (fls,idn)   = blocks !! ix
        frIx        = indexFirstFreeBlock fls (take ix blocks)  -- index
        fr          = fst $ blocks !! frIx                      -- size of free space
        startBlocks = take frIx blocks
        restBlocks  = drop (frIx+1) (take ix blocks ++ [(fls,freeSpace)] ++ drop (ix+1) blocks)
        newBlocks1  = startBlocks ++ [(fls,idn)]                         ++ restBlocks    
        newBlocks2  = startBlocks ++ [(fls,idn)] ++ [(fr-fls,freeSpace)] ++ restBlocks

indexFirstFreeBlock :: Int -> [(Int,Int)] -> Int 
indexFirstFreeBlock fls blocks    
    | blocks     == []  = noFreeBlocks
    | freeBlocks == []  = noFreeBlocks
    | otherwise         = fst $ head freeBlocks       
        where
            freeBlocks  = filter 
                            (\(_,(fr,ids)) -> ((ids == freeSpace) && (fr >= fls))) 
                            (zip [0..] blocks)


workFiles ::  ([(Int,Int)] ->  [(Int,Int)]) -> String -> Int
workFiles dfn = checkSum . idList . dfn . parseDiskMap   

main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 9  (Haskell)"
            day9 <- readFile filename
            putStr "Part one: Resulting filesystem checksum: "
            print $ workFiles moveFileBlocks day9
            putStr "Part two: Resulting filesystem checksum: "
            print $ workFiles defrag day9
            putStrLn "0K.\n"

--  End of code
