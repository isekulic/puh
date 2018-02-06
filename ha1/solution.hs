import Data.Char

{-
  Checks whether the length of a given string s, /len/, satisfies min <= len <= max
-}
strlenInRange s min max 
  | min < 0 || max < 0 = error "String length cannot be a negative number"
  | otherwise          = length s >= min && length s <= max

{-
  Given a list of numbers, an index and a value, checks whether the list element at the given index
  is greater than the value. An call with an index out of bounds return False.
-}
isHereAGreater xs i val
  | i < 0 || i >= length xs = False
  | otherwise               = xs !! i > val

{-
  Reads two lines from the user - a sentence and a taboo word. Prints the sentence with all occurrences
  of the taboo word filtered out.
-}
wordFilter = do
  line <- getLine
  word <- getLine
  let newLine = unwords [w | w <- words line, w /= word]
  putStrLn newLine

{-
  Takes three numbers (or anything else that can be compared) and returns a list of the same numbers
  in ascending order. NEVER do this in real life. Please.
-}
ord3 a b c = if a < b then (if b < c then [a, b, c] else (if a < c then [a, c, b] else [c, a, b] ))
             else (if a > c then (if c > b then [b, c, a] else [c, b, a]) else [b, a, c])

{-
  Takes a vector as a tuple (i, j) and returns its Euclidean norm.
-}
norm v = sqrt (fst v ^ 2 + snd v ^ 2)

{-
  Takes two vectors as tuples (i1, j1), (i2, j2) and returns their sum, a new vector.
-}
add x y = (fst x + fst y, snd x + snd y)

{-
  Takes a vector as a tuple (i, j) and a scalar s and returns a new vector which is the
  result of multiplying the two.
-}
scalarMult v s = (fst v * s, snd v * s)

{-
  Takes two vectors as tuples (i1, j1), (i2, j2) and returns their dot (scalar) product.
-}
dot x y = fst x * fst y + snd x * snd y

{-
  Takes two characters and returns a list of every character in that range, in ascending
  order, paired with its ascii code, as (char, ascii). If the second character is smaller
  than the first one, an empty list is returned.
-}
asciiRange from to = zip [from..to][ord from..ord to]

-- A helper function for incrementing a character by i increments
incChar c i = chr (ord c + i)

{-
  Takes a string and increments every character by i. The i can be negative, in which case
  a decrement occurs.
-}
incn i xs = [incChar x i | x <- xs]