-- lecture 5 - recursions
import Data.List
import Data.Char

-- Exercises
-- 1.1
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- 1.2
headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ([]:xss) = headsOf xss
headsOf ((x:xs):xss) = x : headsOf xss

-- 2.1
modMult _ _ [] = []
modMult n m (x:xs) = x * (n `mod` m) : modMult n m xs

-- 2.2
addPredecessor [] = []
  addPredecessor xs = add 0 xs
    where add _ [] = []
          add p (x:xs) = p + x : add x xs

-- 3.1
equalTriplets [] = []
equalTriplets ((x,y,z):xs) 
  | x == y && x == z = (x,y,z) : equalTriplets xs
  | otherwise = equalTriplets xs

-- 3.2
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n-1) a

-- 4.1
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' 0 xs = xs
drop'' n xs 
  | n > 0 = drop' n xs
  | otherwise = reverse $ drop' (abs n) (reverse xs)


-- 5.1
eachThird :: [a] -> [a]
eachThird (x:y:z:xs) = z : eachThird xs
eachThird _          = []

-- 6.1
length' :: [a] -> Int
length' [] = 0
length' xs = fooLen xs 0
  where fooLen []     n = n
        fooLen (y:ys) n = fooLen ys (n+1)  


