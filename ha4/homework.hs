{- PuH - Homework Assignment 4
 - October 2014.
 -}

import Data.List
import Data.Char
import Data.Function (on)

{- Task 1
 - Calculate factorial of each element in list 0..n-1 and then sum them all up.
 -}
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

leftFactorial :: Integer -> Integer
leftFactorial 0 = 0
leftFactorial n = foldl1 (+) (map factorial [0..n-1])


{- Task 2
 - 5 * any even number will give you 1 zero. Computing how many factor five are there 
 - for each number n..1. e.g. 10 = 5*2 (will return 1), 50 = 5^2 * 2 (return 2)
 - maybe implementation with lists would be faster? does haskell store whole list?
 - it would be faster if i hadn't used recursion at all :D
 -}
factorialZeroes :: Int -> Int
factorialZeroes 0 = 0
factorialZeroes n 
  | n `mod` 5 == 0 = howManyFives n + factorialZeroes (n-5)
  | otherwise = factorialZeroes (n-1)

howManyFives :: Int -> Int
howManyFives n 
  | n `mod` 5 == 0 = 1 + howManyFives (n `div` 5)
  | otherwise = 0

{- Task 3
 - Splitting a list on (Left, Right) and calling 'myZip'
 -}
interleave :: [a] -> [a]
interleave [] = []
interleave xs = myZip $ splitAt splittingAt xs
  where splittingAt = ceiling $ (fromIntegral $ length xs)/2

-- like zip, but takes care of lists where one has extra element 
myZip :: ([a], [a]) -> [a]
myZip ([],_) = []
myZip (x:[],[]) = x:[]
myZip (x:xs, y:ys) = x:y: myZip (xs,ys)

{- Task 4
 - Simple recursion.
 -}
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:[]) = []
pairs (x:xs) = [(x, a) | a <- xs] ++ pairs xs

{- Task 5
 - Checking element by element from 'inits xs' with 'isItShortest'. 
 - 'isItShortest' checks whether first list contains n times second list.
 -}
shortestSub :: Eq a => [a] -> [a]
shortestSub [] = []
shortestSub xs = head [ shortest | shortest <- inits xs, isItShortest xs shortest ]

isItShortest :: Eq a => [a] -> [a] -> Bool 
isItShortest xs shortest
  | null shortest = False
  | null xs = True 
  | shortest == fst splited = isItShortest (snd splited) shortest
  | otherwise = False 
    where splited = splitAt (length shortest) xs

{- Task 6
 - Functions are mostly self - explainatory.
 -}
type Timestamp = [Int]

-- 6.a
isValidTimestamp :: Timestamp -> Bool
isValidTimestamp [] = False 
isValidTimestamp (s:[]) = checkMorS s
isValidTimestamp (m:s:[]) = checkMorS m && checkMorS s
isValidTimestamp (h:m:s:[]) = h `elem` [0..23] && checkMorS m && checkMorS s
isValidTimestamp _ = False

-- checks whether minutes and seconds are in ok interval
checkMorS :: Int -> Bool
checkMorS x = x `elem` [0..59]

-- 6.b
timestampToSec :: Timestamp -> Int
timestampToSec ts
  | not $ isValidTimestamp ts = error "Invalid timestamp."
  | otherwise = convertTime ts

convertTime :: Timestamp -> Int
convertTime (s:[]) = s
convertTime (m:s:[]) = m*60 + s
convertTime (h:m:s:[]) = h*60*60 + m*60 + s

-- 6.c
timeDiff :: Timestamp -> Timestamp -> Int
timeDiff xts yts 
  | not $ isValidTimestamp xts && isValidTimestamp yts = error "Invalid timestamp."
  | otherwise = abs $ convertTime xts - convertTime yts 

{- Task 7
 -
 -}
-- 7.a
counts :: Ord a => [a] -> [(a, Int)]
counts xs = [ (head c, length c) | c <- group $ sort xs ]

-- 7.b
group' :: Eq a => [a] -> [[a]]
group' xs = [ replicate (countIt x xs) x | x <- nub xs ]
-- counts how many x's are in xs
countIt :: Eq a => a -> [a] -> Int
countIt x xs = length $ filter (== x) xs 

-- 7.c 
counts' :: Eq a => [a] -> [(a, Int)]
counts' xs = [ (head c, length c) | c <- group' xs ]

{- Task 8
 - b part sounds awesome.
 -}
type Grid = [String]

lightsOutLite :: Grid -> Int
lightsOutLite grid 
  | isItBad grid = error "Broken grid!"
  | otherwise = foldl1 (+) (map f grid )
    where f = countIt '1'

-- checks if the grid is bad
isItBad :: Grid -> Bool
isItBad grid = 1 /= (length $ groupBy ((==) `on` length) grid)

{- Task 9
 - every one of four "edit ways" is implemented in other function. 
 - after adding all edited strings together, we discard duplicates and sort the lsit
 -}
oneEdits :: String -> [String]
oneEdits xs = sort $ nub ( [xs]
           ++ [ dropThisOne n xs | n <- [1..length xs]] 
           ++ concat [ permuteThisOne n xs | n <- [0..length xs -1]]
           ++ [ swapThisTwo n xs | n <- [0..length xs -2]]
           ++ concat [ addLettersHere n xs | n <- [0..length xs]])

-- drops element at given index
dropThisOne :: Int -> String -> String
dropThisOne n xs = take (n-1) xs ++ drop n xs 

-- takes index of element which is to be changed and a string
permuteThisOne :: Int -> String -> [String]
permuteThisOne n xs = let (ys, z:zs) = splitAt n xs in
                     [ys ++ c : zs | c <- ['a'..'z'], c /= z ] 

-- swaps nth element and n+1th in a string 
swapThisTwo :: Int -> String -> String
swapThisTwo n xs = let (ys, z:w:zs) = splitAt n xs in
                   ys ++ w:z:zs

-- adds letters to a given position
addLettersHere :: Int -> String -> [String]
addLettersHere n xs = let (ys, zs) = splitAt n xs in
                      [ys ++ c:zs | c <- ['a'..'z']]

-- 9.b
twoEdits :: String -> [String]
twoEdits xs = sort $ concat $ map oneEdits (oneEdits xs)

{- Task 10
 - how does the limit work exacly? :D
 - should i pass limit as m to fromSeed?
 -}
type Seed = Int

fromSeed :: Seed -> Int
fromSeed x0 = (a * x0 + c) `mod` m
  where a = 934857 
        c = 54321
        m = 99999999

guess :: Seed -> Int -> IO Ordering
guess x0 limit = do
  putStr "guess: "
  userNStr <- getLine
  return (userN userNStr `compare` ((fromSeed x0) `mod` limit))
    where userN x = digitToInt $ head x