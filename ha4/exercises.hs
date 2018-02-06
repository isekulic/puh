{- PuH in - class exercises 2014
 - This code is quite ugly, sorry.
 -}

import Data.Char
import Data.List

-- 1.1
headHunter ((x:xs):_) = x
headHunter (xs:(y:ys):_) = y
headHunter (xs:ys:(z:zs):_) = z
headHunter xs = error "no heads to hunt :("

-- 1.2
firstColumn m = [ head c | c <- m ]
-- Checked what happens if the input is not a valid matrix.

-- 1.3
shoutOutLoud :: String -> String
shoutOutLoud xs = unwords [ replicate 3 y ++ ys | (y:ys) <- words xs]

-- 2.1
pad :: String -> String -> (String, String)
pad (x:xs) (y:ys)
  | dif > 0 = (toUpper x:xs, toUpper y:ys ++ replicate dif ' ')
  | otherwise = (toUpper x:xs ++ replicate (abs dif) ' ', toUpper y:ys)
  where dif = length xs - length ys

-- 2.2
median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs

quartiles :: [Int] -> (Double, Double, Double)
quartiles xs = ( median $ fst (split ys)
               , median $ fst$ split (snd (split ys))
               , median $ snd$ split (snd (split ys)) )
  where 
    ys = sort xs
    split ss= splitAt ((length ss) `div` 3) ss


-- 4.1 
foo :: (Show c, Num a, Num b, Eq a, Eq b) => (a, b) -> [c] -> String
foo (a,b) xc = "The pair " ++ bar (a,b) ++ " and the second element of the list is " ++ show (xc!!1)

bar :: (Num a, Num b, Eq a, Eq b) => (a,b) -> String
bar (1,1) = "contains two ones"
bar (1,b) = "containst one one"
bar (a,1) = "contains one one"
bar _     = "does not contain a single one"

