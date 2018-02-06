{- | 
  PuH - Homework Assignment 5
  November 2014
 -}
import Data.List

-- Task 1
intercalate' :: [a] -> [[a]] -> [a]
intercalate' _  (y:[])   = y
intercalate' xs (y:yss) = y ++ xs ++ intercalate' xs yss 

{- Task 2
 - why do i get 2.6500000000000004
 -}
type Probability = Double
type DiscreteRandVar = [(Int, Probability)]

x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

mean :: DiscreteRandVar -> Double
mean []          = 0
mean ((xi,p):xs) = fromIntegral xi * p + mean xs

-- accumulator - style recursion
mean' :: DiscreteRandVar -> Double
mean' xs = accMean xs 0
  where accMean [] acc = acc
        accMean ((xi,p):xs) acc = accMean xs (acc + fromIntegral xi*p)

-- 2.b
variance :: DiscreteRandVar -> Double
variance xx = vari (mean xx) xx
  where vari _  []          = 0
        vari meanx ((xi,p):xs) = (fromIntegral xi-meanx)^2*p + vari meanx xs

-- accumulator - style recursion
variance' :: DiscreteRandVar -> Double
variance' xx = vari xx xx 0
  where vari _  []          acc = acc
        vari xx ((xi,p):xs) acc = vari xx xs (acc + (fromIntegral xi-mean xx)^2*p) 

-- 2.c
probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter _   [] = [] 
probabilityFilter prob ((xi,p):xs)
  | prob <= p = xi : probabilityFilter prob xs
  | otherwise = probabilityFilter prob xs

-- acc-style recursion 
probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' prob xs = pFilter prob xs []
  where pFilter _    []          acc = reverse acc
        pFilter prob ((xi,p):xs) acc = if prob <= p 
                                       then pFilter prob xs (xi:acc)
                                       else pFilter prob xs acc

-- | Task 3
chunk :: Int -> [a] -> [[a]]
chunk n xs 
  | n <= 0         = []
  | n >= length xs = [xs]
  | otherwise      = [take n xs] ++ chunk n (drop n xs) 

-- 3.b
chunkBy :: [Int] -> [a] -> [[a]]
chunkBy [] _ = []
chunkBy (i:is) xs 
  | i >= length xs = [xs]
  | i == 0         = chunkBy is xs
  | otherwise      = [take i xs] ++ chunkBy is (drop i xs)  

-- 3.c
chunkInto :: Int -> [a] -> [[a]]
chunkInto n xs 
  | n <= 0        = []
  | n > length xs = cInto xs 1 
  | otherwise     = cInto xs (length xs `div` n)

-- a bit "modifided" version of 'chuckInto'. to match my idea
cInto :: [a] -> Int -> [[a]]
cInto xs len 
  | len > (length $ dropSome) = [xs]
  | len <= 0                  = []
  | otherwise                 = [take len xs] ++ cInto dropSome len
    where dropSome = drop len xs


-- Task 5
-- | Direct implementation of Euclidean algorithm.
gcd' :: Int -> Int -> Int
gcd' a b 
  | a < 0 || b < 0 = gcd' (abs a) (abs b)
  | a < b          = gcd' b a
  | b == 0         = a
  | otherwise      = gcd' b r 
    where r = a `mod` b

-- 5.b - easy with foldl
gcdAll :: [Int] -> Int
gcdAll []     = error "Cannot compute gcd of an empty list."
gcdAll (x:[]) = x
gcdAll xs     = foldl1 f xs
                where f = (\acc x -> gcd' acc x) 


{-|Task 6
   The 'isBipartite' function takes an unweighted graph represented as an adjacency list
   and checks whether the given graph is a bipartite graph.
 -}
type AdjacencyList = [Int]
type Graph = [AdjacencyList]
type Vertex = Int
type Neighbors = (Vertex, AdjacencyList)
type ColorSet = [Neighbors]

-- | Check whether a graph is bipartite. 
isBipartite :: Graph -> Bool
isBipartite []    = True
isBipartite graph = helpMeFunction (zip [1..] graph) [] [] 

-- | Sorts vertices in suitable color sets. If one doesn't belong anywhere, return False.
helpMeFunction :: [Neighbors] -> ColorSet -> ColorSet -> Bool
helpMeFunction [] _ _ = True
helpMeFunction (n:ngh) red blue 
  | isThisColorGood n red  = helpMeFunction ngh (n:red) blue
  | isThisColorGood n blue = helpMeFunction ngh red (n:blue)
  | otherwise              = False

-- | Checks whether given vertex (as touple) can be part of given color set 
isThisColorGood :: Neighbors -> ColorSet -> Bool 
isThisColorGood _ []    = True
isThisColorGood (v, aList) color
  | isAnyInColor aList (map fst color) = False
  | otherwise = True

-- | Checks if any element in first list is in the second list. 
isAnyInColor :: AdjacencyList -> [Vertex] -> Bool
isAnyInColor [] _ = False 
isAnyInColor (x:xs) ys
  | x `elem` ys = True
  | otherwise   = isAnyInColor xs ys 

-- | Task 7
permutations' :: Eq a => [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [ x : px | x <- xs, px <- permutations' (xs\\[x]) ]

-- | Task 8
frogJumps :: Int -> Integer 
frogJumps 0 = 0
frogJumps n = 3 * frogJumps (n-1) + 2
