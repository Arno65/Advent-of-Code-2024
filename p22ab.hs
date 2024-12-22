-- Advent of Code 2024 - Day 22 part One and Two
-- Solutions in Haskell
-- (Ter leering ende vermaeck...)
--
-- Part one: the sum of the secret numbers is:            20411980517
-- Part two: the most bananas to get with [-2,1,-1,2] is:        2362
--
-- (cl) by Arno Jacobs, 2024-12-22


-- module AoC2024d22ab where

import Data.Bits (xor,shift)
import Data.List (sort)


-- Some initials
filename :: String
filename = "data/inputDay22_2024.txt"

iterations :: Int
iterations = 2000

secretNumberSequence :: Int -> Int
secretNumberSequence step0 = (xor step2 (shift step2  11)) `mod` 16777216
    where 
        step1 = (xor step0 (shift step0   6))  `mod` 16777216
        step2 = (xor step1 (shift step1 (-5))) `mod` 16777216

-- Part one ---------------------------------------------------------------
--
generatedSecretNumber :: Int -> Int -> Int
generatedSecretNumber 0  sn = sn
generatedSecretNumber ic sn = 
    generatedSecretNumber (ic-1) (secretNumberSequence sn)

partOne :: [Int] -> Int
partOne = sum . map (generatedSecretNumber iterations)

-- Part two ---------------------------------------------------------------
--
-- Convert the four change values to one unique Int
-- {-9..9} has 19 unique digits, so convert base-19 to base-10
to19 :: [Int] -> [Int]
to19 = map (\i -> i+9)  -- {-9..9} to {0..18}

fromDigits :: Int -> [Int] -> Int
fromDigits base = foldl (\x -> \y -> base*x+y) 0 

toBase10 :: [Int] -> Int
toBase10 = fromDigits 19 . to19

toFourDigits :: Int -> [Int]
toFourDigits = toDigits 4

toDigits :: Int -> Int -> [Int]
toDigits 0  _ = []
toDigits dc i = toDigits (dc-1) d ++ [m-9]
    where
        (d,m) = divMod i 19

deltas :: [Int] -> [Int]
deltas []        = []
deltas (_:[])    = []
deltas (x:xs)    = [head xs - x] ++ deltas xs

priceList :: [Int] -> [Int]
priceList = map (\sn -> mod sn 10)

swapUniqueCodes :: [(Int,Int)] -> [(Int,Int)]
swapUniqueCodes []          = []
-- only store the first code (with price) and swap tuple
swapUniqueCodes ((p,c):rcs) = ((c,p):swapUniqueCodes ncs)   
    where
        ncs = filter (\(_,c') -> c' /= c) rcs

generatedSecretNumbersList :: Int -> Int -> [Int]
generatedSecretNumbersList ic sn 
    | ic < 2    = [sn]
    | otherwise = [sn] ++ generatedSecretNumbersList (ic-1) nsn
        where
            nsn = secretNumberSequence sn

-- Enter price list
sequenceList :: [Int] -> [Int] -> [(Int,Int)]
sequenceList (sd:rsdls) dls 
    | length dls < 4    =   []
    | psd == 0          =   sequenceList rsdls (tail dls) -- don't store zero's
    | otherwise         =   [(psd,sdl)]
                        ++  sequenceList rsdls (tail dls)
        where
            sdl = toBase10 $ take 4 dls
            psd = rsdls!!3

priceAndSequences :: Int -> Int -> [(Int,Int)]
priceAndSequences ln n = (sort . swapUniqueCodes . sequenceList ps) $ deltas ps 
    where
        ps = priceList $ generatedSecretNumbersList ln n

sumAllSequences :: [[(Int,Int)]] -> [(Int,Int)]
sumAllSequences sqss = sumSequences $ sort $ concat sqss

sumSequences :: [(Int,Int)] -> [(Int,Int)]
sumSequences []             = []
sumSequences ((c,p):rsqs)   = [(c,p+p')] ++ sumSequences nsqs
    where
        p'   = (sum . map snd . takeWhile (\(c',_) -> c' == c)) rsqs
        nsqs = dropWhile (\(c',_) -> c' == c) rsqs

partTwo :: [Int] -> (Int,Int)
partTwo sns = (     maximum 
                .   swapTuples 
                .   sumAllSequences 
                .   map (priceAndSequences iterations)) sns
    where
    --  swapTuples :: [(a,b)] -> [(b,a)] 
        swapTuples = map (\(x,y) -> (y,x))

            
main :: IO ()
main = do   putStrLn "Advent of Code 2024 - day 22  (Haskell)"
            day22 <- map read <$> lines <$> readFile filename
            putStr "Part one: the sum of the secret numbers is:            "
            print $ partOne day22
            let (p,c) = partTwo day22
            putStr "Part two: the most bananas to get with "
            putStr $ show $ toFourDigits c 
            putStr " is:        "
            print p        
            putStrLn "0K.\n"

-- end of code
