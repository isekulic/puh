-- 1.1
takeThree = take 3
dropThree = drop 3
hundredTimes = replicate 100

-- 1.2
index = zip [0..]
index' = (`zip` [0..])

-- 1.3 
divider :: Int -> String
divider = (`replicate` '=')

-- 2.1
applyOnLast f xs ys = f (last xs) (last ys)

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 = applyOnLast (addThree 100) 

addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

-- 2.2
applyManyTimes n f x 
  | n <= 0 = x
  | otherwise = applyManyTimes (n-1) f (f x)

applyTwice = applyManyTimes 2 

-- 3.1
listifyList :: [a] -> [[a]]
listifyList = map (:[])

-- 3.2
--cutoff :: Int -> [Int] 
--cutoff n = map (min n)

-- 4.1
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum $ map (^2) $ filter even xs

-- 4.2
freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (== x) xs

-- 4.3
--freqFilter :: Eq a => Int -> [a] -> [a]
--freqFilter n 

-- 5.1
withinInterval n m xs = filter (`elem` [n..m]) xs 

-- 5.2
sndColumn :: [[a]] -> [a]
sndColumn m = map (\(x:y:_) -> y) m

-- 5.3 
canoinicalizePairs :: Ord a => [(a,a)] -> [(a,a)]
canoinicalizePairs xs = map (\(x,y) -> if x < y then (x,y) else (y,x)) $
  filter (\(x,y) -> x /= y) xs