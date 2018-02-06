{-
  PuH - Homework Assignment 7
  November 2014.
-}

import Data.List 
import Data.Char
import Data.Ord (comparing)
import Data.Maybe (fromJust)

main :: IO ()
main = undefined

-- Task 1
-- 1.a
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x       = x : takeWhile' f xs
  | otherwise = []

-- 1.b
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f xx@(x:xs)
  | f x       = dropWhile' f xs
  | otherwise = xx 
-- dropWhile' (>0)

-- 1.c
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 


-- Task 2 - sorting a list of very long strings 
efficientSortBy :: t -> [[a]] -> [[a]]
efficientSortBy f xs = map snd (sortBy mySorting [(length x, x) | x <- xs])

mySorting :: Ord a => (a, t) -> (a, t1) -> Ordering
mySorting (lx, x) (ly, y)
  | lx < ly  = LT
  | lx > ly  = GT 
  | lx == ly = EQ


-- Task 3 - Stemming
stemmer1 :: String -> String 
stemmer1 xs = reverse $ stemIt (reverse xs) []

stemIt :: String -> String -> String
stemIt []     suf = reverse suf
stemIt (x:xs) suf 
  | isVowel x = if length xs > length suf then xs 
                else suf ++ x:xs 
  | otherwise = stemIt xs (x:suf)

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

-- 3.b
-- suffixes = ["ing", "s", "es", "er"]

stemmer2 :: [String] -> String -> String 
stemmer2 suffixes xs = stemIt2 (sortBy (flip (comparing length)) suffixes) (reverse xs) 

-- gets list of suffixes sorted by length (longer ones being first)
stemIt2 :: [String] -> String -> String 
stemIt2 []      xs = reverse xs
stemIt2 (s:suf) xs 
  | reverse s == take (length s) xs && length s <= length (drop (length s) xs) = reverse $ drop (length s) xs
  | otherwise = stemIt2 suf xs 

-- 3.c
suffixes = ["ing", "s", "es", "er"]

stemmer3 :: [String] -> String -> String 
stemmer3 suffixes xs = stemIt3 (sortBy (flip (comparing length)) suffixes) (reverse xs)

stemIt3 :: [String] -> String -> String   
stemIt3 []      xs = stemmer1 (reverse xs) 
stemIt3 (s:suf) xs 
  | reverse s == take (length s) xs && length s <= length (drop (length s) xs) = reverse $ drop (length s) xs
  | otherwise = stemIt3 suf xs


-- 3.d
pairs :: [(String, String)]
pairs = [("driving", "driv"), ("fools", "fool"), ("teacher", "teach")]

testStemmer :: [(String, String)] -> (String -> String) -> Double 
testStemmer px stemmer = 100 * howMany px stemmer 0 / genericLength px

howMany :: [(String, String)] -> (String -> String) -> Double -> Double
howMany [] _ n = n
howMany ((word, stem):px) f n 
  | f word == stem = howMany px f (n+1)
  | otherwise = howMany px f n

-- 3.e 
stemText :: (String -> String) -> (String -> Bool) -> String -> String 
stemText stemmer p xs = unwords $ filter p (map stemmer (words xs))


-- Task 4
-- 4.a centroid 
type Point = (Double, Double)
centroid :: [Point] -> Point
centroid [] = error "Cannot calculate centroid of zero points..."
centroid points = (sum (map fst points) / genericLength points, sum (map snd points) / genericLength points) 

-- 4.b 
groupByDist :: [Point] -> [Point] -> [(Point, [Point])]
groupByDist _  [] = error "Cannot group around less then one point"
groupByDist xs ys = groupIt xs ys (initZs ys)

-- initialize list to be modified in form (y, ys)
initZs :: [Point] -> [(Point, [Point])]
initZs ys = [ (y, []) | y <- ys ]

groupIt :: [Point] -> [Point] -> [(Point, [Point])] -> [(Point, [Point])]
groupIt []     _      zs = zs
groupIt (x:xs) (y:ys) zs = groupIt xs (y:ys) (appendToZs zs x (findMinDist x (y:ys) y))

-- returns y with min distance
findMinDist :: Point -> [Point] -> Point -> Point 
findMinDist _ []     yMin = yMin 
findMinDist x (y:ys) yMin
  | distance x y < distance x yMin = findMinDist x ys y  
  | otherwise = findMinDist x ys yMin

distance :: Point -> Point -> Double
distance (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2) 

appendToZs :: [(Point, [Point])] -> Point -> Point -> [(Point, [Point])]
appendToZs zs x y = [ if (x1,y1) == y then ((x1,y1), x:ss) else ((x1,y1), ss) | ((x1,y1),ss) <- zs] 

-- 4.c k-means 
cluster :: [Point] -> Int -> Int -> [(Point, [Point])]
cluster xs k i 
  | null xs = error "Cannot cluster for no points"
  | length xs < k = error "The number of groups cannot be greater then the number of elements"
  | otherwise = clusterIt xs (take k xs) i 

clusterIt :: [Point] -> [Point] -> Int -> [(Point, [Point])]
clusterIt xs centrs i 
  | i <= 0 = groupByDist xs centrs 
  | otherwise = clusterIt xs (map (centroid . snd) (groupByDist xs centrs)) (i-1)



-- Task 6 Do you speak?
doYouSpeak :: [String] -> String -> Bool
doYouSpeak _ [] = True
doYouSpeak [] _ = False
doYouSpeak xss ys 
  | contains xss ys = doYouSpeak xss (removeIt (sortBy (flip compare) xss) ys)
  | otherwise = False

removeIt :: [String] -> String -> String
removeIt (xs:xss) ys 
  | map toLower xs == take (length xs) ys = drop (length xs) ys
  | otherwise = removeIt xss ys  

contains :: [String] -> String -> Bool 
contains xss ys = not $ null [xs | xs <- xss, map toLower xs == take (length xs) ys] 


-- Task 7 - histogram
histogram :: String -> IO ()
histogram xs = do
  printingIt (countingIt makeHist filteredXs) 
  print ['a'..'z']
    where filteredXs = map toLower $ filter isAlpha xs

-- counting number of letter occurrences in xs
countingIt :: [Int] -> String -> [Int]
countingIt = foldl update 

-- initialize empty histogram
makeHist :: [Int]
makeHist = replicate 26 0

-- updates histogram with a given letter
update :: [Int] -> Char -> [Int]
update hist x = [ if fromJust (elemIndex x ['a'..'z']) == i then h+1 else h | (i, h) <- zip [0..] hist ]

-- prints in that retarded form
printingIt :: [Int] -> IO ()
printingIt hist = actualPrint hist (maximum hist)


actualPrint :: [Int] -> Int -> IO ()
actualPrint _    0 = print "--------------------------"
actualPrint hist maxh = do 
  print [ if i `elem` elemIndices maxh hist then '*' else ' ' | i <- [0..25] ]
  printingIt subMax  
    where subMax = [ if h /= maxh then h else h -1 | h <- hist]
