-- 1.1
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..] 

-- 1.2
filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

-- 2.1
-- maxDiff :: [Int] -> Int 
-- maxDiff xs = maximum . map f . makePairs xs
--   where f (x,y) = x - y

-- makePairs :: [Int] -> [(Int, Int)]
-- makePairs [] = []
-- makePairs (x:[]) = []
-- makePairs (x:y:xs) = (x,y) : makePairs xs

-- 4.1
elem1 x = foldr (\y a -> a || x == y) False 

-- 4.2
reverse' = foldr (\x a -> a ++ [x]) []

-- 5.1
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) [] 

-- 5.2
-- sumEven :: [Integer] -> Integer 
-- sumEven = foldl (\a (x,y) -> if even x then a + y) . zip [0..]