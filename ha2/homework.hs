{- PuH - Homework Assignment 2
 - http://www.fer.unizg.hr/_download/repository/puh-ha2%5B1%5D.pdf
 - October 2014.
 -}
--module Test where 
import Data.List
import Data.Char
import System.Environment


{- Task 1.a 
 - Taking first letter of each word, makin' it big and adding back to the tail of the word
 - words are in list (xs) we got by calling func. words on our input string 
 -}
toTitleCase string = unwords [ (toUpper $ head xs):(tail xs) | xs <- words string ]

{- Task 1.b
 - Solution follows the same logic as 1.a, with if-then-else added. 
 - Upping or word if it's the first in sentence, even though it may be in list of word that are to stay lowercase  
 -}
toTitleCase' string listOfLowercases = 
  unwords [ if xs `elem` listOfLowercases && ifnotFirst xs then lowerEm xs else upEm xs | xs <- words string ]
  where 
    ifnotFirst s = s /= (head $ words string)
    lowerEm s = (toLower $ head s):(tail s)
    upEm s = (toUpper $ head s):(tail s)

{- Task 2
 - Simple recursion that stops when n == 0. 
 - In each call we are passin list trimed by 1 and decrementing n by 1 
 -}
trimN trimThatList n = 
  if 2*n > length trimThatList || n <= 0 then trimThatList
  else trimN (tail $ init trimThatList) (n - 1) 

{- Task 3
 - We get file path with getArgs, give that path to readFile function and get the file context
 -}
main = do
  filePath <- getArgs
  string <- readFile  $ unwords filePath 
  putStrLn [ toUpper c | c <- string ]

{- Task 4
 -
 -}
onlyDivisible string n = if n > 0 then [ c | (position, c) <- zip [0..] string, position `mod` n == 0]
                                  else error "n must be positive!"

{- Task 5
 - In order to avoid repetition of elements, we use 'tails' and select next elem. Then recursively select n-1 elem from the rest of tail.
 - Check whether triangle is degenerative is pretty sloppy written - fix inc if there is time
 -}
triangleCounter listOfTuples = length [ ss | ss <- combinations 3 listOfTuples
                              , (fst (ss!!2)) - (fst (ss!!1)) /= (fst (ss!!1)) - (fst (ss!!0)) 
                              && (snd (ss!!2)) - (snd (ss!!1)) /= (snd (ss!!1)) - (snd (ss!!0))]
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

-- Task 6
reverseWords sentenceToReverse = unwords $ reverse $ words sentenceToReverse

{- Task 7
 - 
-}
intersect' firstList secondList = [ c1 | (c1,c2) <- zip firstList secondList, c1 `elem` [c | (a,c) <- zip firstList secondList]]
-- NOT CORRECT - fix inc 
difference firstList secondList = [ c1 | (c1,c2) <- zip firstList secondList, c1 `notElem` [c | (a,c) <- zip firstList secondList]]

{- Task 8
 - a. Recursion that stops if it encounters on 2 elements (rows) of different length or when there is only one element left.
 - funcions are pretty easy to understand i'd say. I know I could put error "" in checkIndex function and similar, but I'm out of time 
 -}
isWellFormed :: [[a]] -> Bool
isWellFormed [[]] = False 
isWellFormed (x:[]) = True -- if list contains only 1 element
isWellFormed (x:xs) = if length x == (length $ head xs) then isWellFormed xs else False 

size matrix = if isWellFormed matrix then (length matrix, length (matrix!!0)) else error "Matrix is malformed"

checkIndex matrix ind = ind >= length matrix || ind < 0 -- if true then it's bad

getElement matrix i j 
  | isWellFormed matrix == False = error "Matrix is malformed"
  | checkIndex matrix i || checkIndex (head matrix) j = error "Index out of bounds"
  | otherwise = matrix!!i!!j

getRow matrix i
  | isWellFormed matrix == False = error "Matrix is malformed"
  | checkIndex matrix i == True = error "Index out of bounds"
  | otherwise = matrix!!i

getCol matrix j 
  | isWellFormed matrix == False = error "Matrix is malformed"
  | checkIndex (head matrix) j == True = error "Index out of bounds"
  | otherwise = [ row!!j | row <- matrix]

differentSize firstMatrix secondMatrix = 
  length firstMatrix /= length secondMatrix 
  || length (firstMatrix!!0) /= length (secondMatrix!!0)

addMatrices firstMatrix secondMatrix 
  | isWellFormed firstMatrix && isWellFormed secondMatrix == False = error "Matrix is malformed"
  | differentSize firstMatrix secondMatrix = error "Matrices are not of equal size"
  | otherwise = [[getElement firstMatrix i j + getElement secondMatrix i j | j <- [0..(length (head firstMatrix) - 1)]]
    | i <- [0..(length firstMatrix - 1)]]

-- with getCol take cols one by one and put em in rows
transpose' matrix 
  | isWellFormed matrix == False = error "Matrix is malformed"
  | otherwise = [ getCol matrix j | j <- [0..((length (head matrix)) - 1)]]

