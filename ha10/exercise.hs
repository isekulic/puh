import Data.Foldable
import Prelude hiding (foldr)

data Breed = Beagle | Husky | Pekingese deriving (Eq, Ord, Show, Read)

data Dog = Dog {
  dogName :: String, 
  dogBreed :: Breed, 
  dogAge   :: Int  
} deriving (Eq, Ord, Show, Read)

-- data Person = Person {
  
-- }

class Ageing a where
  currentAge :: a -> Int 
  maxAge     :: a -> Int 
  makeOlder  :: a -> a 

-- instance Ageing Person where 
--   currentAge  = age 
--   makeOlder p = p { age = age p +1 }
--   maxAge    _ = 123

-- instance Ageing Dog where
--   currentAge  = age
--   makeOlder p = p { age = age p +1 }
--   maxAge    _ = 


-- compareRelativeAge :: (Ageing a, Ageing b) => a -> b -> Ordering

peso = Dog "peso" Husky 10

class Nameable a where 
  name :: a -> String 

instance Nameable Dog where
   name d = dogName d ++ "the Dog" 


-- 2.
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show, Eq)


class Takeable t where
  takeSome :: Int -> t a -> [a]

instance Takeable [] where
  takeSome = take 

drvo = Node 1 (Node 2 Null Null) (Node 3 Null Null)

instance Takeable Tree where
  takeSome n t = take n (goToList t)

-- goToList :: Tree -> []
goToList Null = [] 
goToList (Node x l r) = goToList l ++ x : goToList r





instance Foldable Tree where 
  foldr f z Null         = z
  foldr f z (Node x l r) = x `f` foldr f (foldr f z r) l
-- e 4
sumPositive :: (Foldable t, Num a) => t a -> a 
sumPositive = Data.Foldable.foldr 