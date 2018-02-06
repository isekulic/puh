-- 1.1
import Data.Maybe

data Sex = Male | Female deriving (Show, Read, Eq, Ord)

data Person2 = Person2 {
  personId2 :: String,
  forname2 :: String,
  surname2 :: String, 
  sex2 :: Sex, 
  mother2 :: Maybe Person2, 
  father2 :: Maybe Person2,
  partner2 :: Maybe Person2,
  children2 :: [Person2] 
} deriving (Show, Read, Eq, Ord)

john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane = Person2 "234" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
ann = Person2 "345" "Ann" "Doe" Female Nothing Nothing Nothing [jane]

partnersMother :: Person2 -> Maybe Person2
partnersMother p = case partner2 p of 
  Just p2 -> mother2 p2
  Nothing -> Nothing

-- 1.2
-- parentCheck :: Person2 -> Bool
-- parentCheck p = p `elem` (children2 $ fromMaybe Nothing $ mother2 p)

-- 2.1
data MyList a = Empty | Cons a (MyList a) deriving (Show, Read, Ord, Eq)

l1 = 1 `Cons` Empty
lw = 1 `Cons` (2 `Cons` Empty)

infixr 5 -+-
(-+-) = Cons

listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (Cons a _) = Just a


-- defining a binary tree
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show, Eq)

intTree :: Tree Int 
intTree = Node 1 (Node 2 Null Null) (Node 3 Null Null)

sumTree :: Tree Int -> Int 
sumTree Null                = 0
sumTree (Node x left right) = x + sumTree left + sumTree right

treeElem :: Eq a => a -> Tree a -> Bool
treeElem _ Null = False
treeElem x (Node y left right) 
  | x == y    = True
  | otherwise = treeElem x left || treeElem x right


  -- treeMax :: Ord a => Tree a -> a
  -- treeMax t@(Node x _ _) = findMax t x 
  --   where findMax Null x                = x
  --         findMax (Node y left right) x = if y > x then findMax 






main :: IO ()
main = undefined

