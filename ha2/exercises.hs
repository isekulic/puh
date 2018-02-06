import Data.Char
import Data.List 

-- 2.1
fun1 lista = tail $ init $ init $ init lista
--2.2
initals s1 s2 = [head s1] ++ "." ++ [head s2] ++ "."
--2.3
concat3 s1 s2
 | length s1 > length s2 = s1 ++ s2
 | otherwise = s2 ++ s1
-- 2.4
safeHead s 
  | s == [] = []
  | otherwise = head s
-- 2.5
hasDuplicates a = not (length a == length (nub a))

-- 3.1
doublesFromTo a b = [x*2 | x <- [a..b] ++ [b..a]]
-- 3.2
caesarCode n s 
  | n <= 0 = s
  | otherwise = caesarCode (n - 1) [ succ $ toLower c | c <-s, c /= ' ' ]

-- 4.1
letterCount xs = length [ x | x <- words xs, length x > 3]
-- 4.2
isPalindrome xs = x == reverse x
  where x = [ toUpper x | x <- xs, x /= ' ']
-- 4.3
flipp xss = concat [ x | x <- [ reverse xs | xs <- reverse xss]]

--5.1
inCircle r x y = [ (a,b) | a <- [-10..10], b <- [-10..10], (a - x)^2 + (b - y)^2 <= r^2]
-- 5.2
steps xs = [(x1,x2) | (x1,x2) <- zip xs (tail xs)]

-- 6.1
indices x xs = [  indx | (indx, c) <- zip [1..] xs, x == c ]
-- 6.2
showLineNumbers s = unlines [ (show num) ++ " " ++ line | (num, line) <- zip [1..] (lines s) ]
-- 6.3 a
haveAlignment xs ys = not $ null [ (a, b) | (a, b) <- zip xs ys, a == b ]
-- 6.3 b
common xs ys = concat [ [a] | (a,b) <- zip xs ys, a == b ]

