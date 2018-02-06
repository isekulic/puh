{- PuH - HA 8
  December 2014
-}

import Prelude hiding (Right, Left)
import Data.Bits

{-
  Task 1. Gonna enslave some turtles.
 -}
-- |x and y coordinates
type Position = (Integer, Integer)
data Orientation = Left | Right | Up | Down deriving (Eq, Show)

-- |Clockwise and Counterclockwise
data TurnDir = CW | CCW deriving (Eq, Show)


-- |Data type for a turtle
data Turtle = Turtle
  { position   :: Position
  , orientation  :: Orientation } deriving (Eq, Show)

-- |Creates a new turtle at (0,0) facing upwards
newTurtle = Turtle { position = (0, 0), orientation = Up }

-- |Moves given turtle in the direction it si currently facing
move :: Integer -> Turtle -> Turtle
move d trtl 
  | d < 0 = error "Turtles cannot move backwards..."
  | otherwise = trtl { position = updatePosition (position trtl) (orientation trtl) }
    where updatePosition (x,y) Up    = (x,y+d)
          updatePosition (x,y) Down  = (x,y-d)
          updatePosition (x,y) Right = (x+d,y)
          updatePosition (x,y) Left  = (x-d,y)

-- |Changes the turtle's orientation
turn :: TurnDir -> Turtle -> Turtle
turn c trtl = trtl { orientation = updateOrientation c (orientation trtl) }

updateOrientation :: TurnDir -> Orientation -> Orientation
updateOrientation CW Left   = Up
updateOrientation CW Up     = Right
updateOrientation CW Right  = Down
updateOrientation CW Down   = Left
updateOrientation CCW Left  = Down
updateOrientation CCW Down  = Right
updateOrientation CCW Right = Up
updateOrientation CCW Up    = Left


-- |Enables chaining our commads to the turtle. 
runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle []     trtl = trtl
runTurtle (f:fs) trtl = runTurtle fs (f trtl)



{-
  Task 2. Walking over the trees.
 -}

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)

testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

-- 2.a
-- |Remove those subtrees that do not satisfy the given predicate.
treeFilter :: (a -> Bool) -> Tree a -> Tree a 
treeFilter _ Leaf = Leaf 
treeFilter f (Node x l r) 
  | f x = Node x (treeFilter f l) (treeFilter f r)
  | otherwise = Leaf

-- 2.b
levelMap :: (Int -> a -> b) -> Tree a -> Tree b 
levelMap = applyFtoTree 0  

applyFtoTree :: Int -> (Int -> a -> b) -> Tree a -> Tree b 
applyFtoTree _     _ Leaf         = Leaf
applyFtoTree depth f (Node x l r) = Node (f depth x) (applyFtoTree (depth+1) f l) (applyFtoTree (depth+1) f r)

-- 2.c
-- Finding potentional subtrees and then passing them to isSubtree'
-- There may be simplier way, but this should work aswell
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf _    = True
isSubtree _    Leaf = False 
isSubtree t@(Node x l1 r1) (Node y l2 r2) 
  | x == y = isSubtree' l1 l2 && isSubtree' r1 r2
  | otherwise = isSubtree t l2 || isSubtree t r2

-- Every element must be the same in this function
isSubtree' :: Eq a => Tree a -> Tree a -> Bool
isSubtree' Leaf Leaf = True
isSubtree' Leaf _    = False 
isSubtree' t@(Node x l1 r1) (Node y l2 r2)
  | x == y = isSubtree' l1 l2 && isSubtree' r1 r2
  | otherwise = False 


{-
  Task 3 - 'Date' data type and some functions for working with dates
 -}
data Date = Date 
  { day :: Int
  , month :: Int
  , year :: Int 
  } deriving (Eq, Show)

date :: Int -> Int -> Int -> Maybe Date 
date d m y 
  | isGoodDate d m y = Just (Date d m y)
  | otherwise = Nothing

goodDates = [[1..31], [1..28], [1..31], [1..30], [1..31], [1..30], [1..31], [1..31], [1..30], [1..31], [1..30], [1..31]]
goodMonths = [1..12]

-- |Checks whether you can construct a valid date from 3 integers
isGoodDate :: Int -> Int -> Int -> Bool
isGoodDate d m y 
  | y >= 0 &&
    m `elem` goodMonths &&
    d `elem` goodDates!!(m-1) = True 
  | otherwise = False 


{-
  Task 4. Recursive data type 'Pred' represents a boolean expression.
 -}
data Pred = And Pred Pred | Or Pred Pred | Not Pred | Val Bool 

expr = And ((Or (Val True)) (Not (Val True))) (Not (And (Val True) (Val False)))

-- |Takes 'Pred' and returns evaluated Bool value.
eval :: Pred -> Bool 
eval (Val a)   = a 
eval (And x y) = eval x && eval y
eval (Or x y)  = eval x || eval y
eval (Not x)   = not $ eval x  

{-
  Task 5. Data type for stack tracing simulation.
  Idea is to construct a rose tree out of stack traces and then print it out
-}
data StackTraceElement = StackTraceElement 
  { className  :: String
  , method     :: String 
  , lineNumber :: Int 
  }

type StackTrace = [StackTraceElement]

data RoseTree a = RoseNode 
  { value    :: a
  , children :: [RoseTree a]  
  } deriving Show

combined :: [StackTrace] -> Int -> String
combined []     _ = ""
combined stList k = printThatTree $ constructTree stList

-- How to define a starting node?
-- constructTree :: [StackTrace] -> RoseTree
constructTree stList = undefined

-- printThatTree :: RoseTree -> String 
printThatTree rTree = undefined

-- putElemInTree :: RoseTree -> StackTraceElement -> RoseTree
putElemInTree rTree el = undefined

-- yep, no time for this now. looks interesting tho, will do it in free time

{-
  Task 6. Playing with Gray code. 
-}
toGrayCode :: (Integral a, Bits a) => a -> a
toGrayCode x = shiftR x 1 `xor` x


fromGrayCode :: (Integral a, Bits a) => a -> a
fromGrayCode x = convertIt x (shiftR x 1)
  where convertIt x 0   = x 
        convertIt x mask = convertIt (x `xor` mask) (shiftR mask 1)


{- 
  Task 7. typeclass 'Truthy' defines types that can be interprated as boolean values.
 -}
class Truthy a where 
  truey  :: a -> Bool 
  falsey :: a -> Bool  

  truey  = not . falsey
  falsey = not . truey

instance Truthy Bool where
  truey x = x

instance Truthy Int where
  truey  0 = False 
  truey  _ = True  

instance Truthy [a] where
  truey [] = False 
  truey _  = True 

if' :: Truthy p => p -> a -> a -> a 
if' cond x y = if truey cond then x else y 

assert :: Truthy p => p -> a -> a 
assert cond x = if truey cond then x else error "Assertion failed"

(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
x &&& y = truey x && truey y

(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
x ||| y = truey x || truey y



main :: IO ()
main = undefined