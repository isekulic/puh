{- PuH - Homework Assignment 6
 - November 2014.
 -}

import Data.Char

main :: IO ()
main = undefined

-- Task 1
-- |Partitions a list using user-provided predicate function.
partition :: [a -> Bool] -> [a] -> [[a]]
partition []     _  = [] 
partition (f:fs) xs = filter f xs : partition fs xs  

-- Task 2
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap [] _ = []
cycleMap fs xs = zipWith (curry doIt) newfs xs
  where newfs = take (length xs) $ cycle fs
        doIt (f,x) = f x

-- Task 3.a
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ seed []     = seed
reduce f seed (x:xs) = reduce f (f seed x) xs 

-- Task 3.b
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 _ []     = error "reduce1 got an ampty list"
reduce1 f (x:xs) = reduce f x xs 

-- Task 3.c
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan _ seed []     = [seed]
scan f seed (x:[]) = [f seed x]
scan f seed (x:xs) = fsx : scan f fsx xs
  where fsx = f seed x

-- Task 4.a
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce _ seed []     = seed
rreduce f seed (x:xs) = x `f` rreduce f seed xs

-- Task 4.b
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ []     = error "rreduce1 got an empty list"
rreduce1 f (x:xs) = rreduce f x xs

-- Task 4.c
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan _ seed []     = [seed] 
rscan f seed xs
  | length xs == 1 = [f (head xs) seed]
  | otherwise = rscan f fsx (init xs) ++ [f (last xs) seed]
    where fsx = f (last xs) seed

-- Task 5
y' :: Double -> Double -> Double
y' y x = (y + x/y)/2

type Tolerance = Double
newton :: Tolerance -> Double -> Double
newton t n 
  | n < 0     = error "can't get sqrt of negative number"
  | otherwise = helpF t n (y' (n/2) n)

helpF t y n
  | t > abs (y - y' y n) = y
  | otherwise = helpF t y (y' y n) 


-- Task 6
-- isHappy :: Int -> Bool
-- isHappy n = 1 == (sum $ map (^2) [ digitToInt c | c <- show n ])

-- Task 7
-- split :: [a] -> ([a], [a])
-- split xs =  