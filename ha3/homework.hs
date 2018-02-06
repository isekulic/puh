{- PuH - Homework Assignment 3
 - October 2014.
 -}
 
module Test where
import Data.List
import Data.Char
import Data.Ord

{- Task 1
 - Recursion stops when one of the lists becomes empty. 
 -}
interleave :: [a] -> [a] -> [a]
interleave _ [] = []
interleave [] _ = []
interleave (x:xs) (y:ys) = x:y:interleave xs ys

{- Task 2
 - First check if indices are suitable for our 'slice'. 
 - Then simply take elements witch are between i and j using list comprehension.
 -}
slice :: Int -> Int -> [a] -> [a]
slice fint sint xs  
  | length xs < j || j < 0 || i < 0 = error "Slice index out of range"
  | otherwise = [a | (a,b) <- zip xs [0..], b >= i, b <= j]
     where 
      i = min fint sint
      j = max fint sint
	  
{- Task 3
 - Each call of decamel takes string till next uppercase letter and 
 - calls decamel providing rest of the string (from where uppercase letter is till end)
 -}
decamel :: String -> String
decamel [] = []
decamel (x:xs)
  | not $ and $ map (isAlpha) xs = error "this ain't camel" 
  | otherwise = unwords [x : remains, decamel $ drop (length remains) xs] 
    where remains = takeWhile (`elem` ['a'..'z']) xs
{- Task 4
 - Both 'count' and 'removeUniques' are pretty much self - explanatory.
 -}
count :: Eq a => [a] -> a -> Int
count xs x = length [ c | c <- xs, c == x]
-- 4.b
removeUniques :: Eq a => [a] -> [a] 
removeUniques xs = [x | x <- xs, notUnique x]
  where notUnique a = length [ c | c <- xs, c == a] >=2

{- Task 5
 - Checking whether we need to cycle our mask or not. If yes, call mask once again with sufficient length.
 - If not, zip string and mask and return elements from string which have their second touple member 1.
 -}
type Mask = String

mask :: String -> Mask -> String
mask _ [] = []
mask xs m = 
  if length m < length xs then mask xs (take (length xs) $ cycle m) 
  else [ a | (a,b) <- zip xs m, b == '1']

{- Task 6
 - First, we make a list of touples (distance from this to my point, name of friend).
 - Then we sort that list by the first element (distance) and take name from the first element (the closest person) 
 -}
type Point = (Int, Int)
type Friend = (Point, String)

distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt $ fromIntegral ((x2-x1)^2 + (y2-y1)^2) 

findFriend :: Point -> [Friend] -> String
findFriend _ [] = error "Nobody exists to be your friend."
findFriend myPoint listFriends = snd $ head $ sortBy (comparing fst) [ (distance myPoint a, b) | (a,b)<-listFriends ]

{- Task 7
 - In mulTable function 'multy' is called with [[]], which is correct form and size of expected return value, but all elements are the same. 
 - 'multy' recursively generates table by multiing each element with suitable number.
 -}
mulTable :: Int -> [[Int]]
mulTable n 
  | n < 1 = error "Given number lesser then 1"
  | otherwise = multy (take n $ cycle [[1..n]]) 0

multy :: [[Int]] -> Int -> [[Int]]
multy table n 
  | n == length table = []
  | otherwise =  [ (n+1)*el | el <- table!!n ] : multy table (n+1)

-- 7.b
-- If given arguments are suitable for task, we call recursive function which simply adds ' ' to our list as many times it needs.
leftpad :: Show a => Int -> a -> String
leftpad howMany showIt 
  | howMany < 0 = error "Cannot pad to negative length"
  | howMany < length didShowIt = error "correct it"
  | otherwise = recurLeftpad (howMany - length didShowIt) didShowIt
  where didShowIt = show showIt
-- 'recurLeftpad' recieves integer which represents how many ' ' shall we put in our string and that string 
recurLeftpad :: Int -> String -> String
recurLeftpad 0 xs = xs
recurLeftpad n xs = ' ' : recurLeftpad (n-1) xs

-- 7.c
prettyTable :: Show a => [[a]] -> IO ()
prettyTable matrix = mapM_ putStrLn [intercalate " " (showRow c) | c <- matrix]

showRow :: Show a => [a] -> [String]
showRow xs = [show c | c <- xs]


{- Task 9
 - This is very messy...sorry. There probably is a better way 
 -}
wc :: FilePath -> IO ()
wc fPath = do 
  wholeFile <- readFile fPath
  putStrLn ((show $ length $ (lines wholeFile)) ++ " "
    ++ (show (allWords wholeFile)) ++ " "
    ++ (show (allChars wholeFile)))
    where 
      allWords f = foldl1 (+) (map length (map words (lines f)))
      allChars f = foldl1 (+) (map countChars (map words (lines f)))

countChars :: [String] -> Int
countChars xss = foldl1 (+) (map length xss)

-- 9.b - using function from task 1
paste :: FilePath -> FilePath -> IO ()
paste fPath1 fPath2 = do
  wholeFile1 <- readFile fPath1
  wholeFile2 <- readFile fPath2
  printingIt $ interleave (lines wholeFile1) (lines wholeFile2)

printingIt :: [String] -> IO ()
printingIt [] = return ()
printingIt (x:y:ss) = do
  putStrLn (x ++ "   " ++ y)
  printingIt ss

-- 9.c
cut :: String -> Int -> FilePath -> IO ()
cut deli part fPath = do
  wholeFile <- readFile fPath
  mapM_ putStrLn [ c!!(part-1) | c <- (map f (lines wholeFile))]
    where f = splitIt (deli!!0)

splitIt :: Char -> String-> [String]
splitIt _ [] = []
splitIt deli (x:xs) = [x:tookIt] ++ (splitIt deli (drop (length tookIt + 1) xs))
  where tookIt = takeWhile (/= deli) xs

 


