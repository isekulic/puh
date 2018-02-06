import Data.Char
import Data.List

-- Task 1 
f_err = error "String length cannot be a negative number."
strlenInRange str x y | x < 0 = f_err
					  | y < 0 = f_err
					  | y-x == length str = True
					  | otherwise = False

-- Task 2
isHereAGreater listOfNumbers index value
				| length listOfNumbers <= index = False
				| listOfNumbers !! index > value = True
				| otherwise = False

-- Task 3
wordFilter = do
	userSentence <- getLine
	userWord <- getLine
	putStrLn (unwords ([element | element <- words userSentence, element /= userWord]))
										 
-- Task 4 - not nice, I know...can't really explain it, algorithm is self explanatory pretty much
ord3 x y z = 
	if x<y then 
		if x<z then 
			if y<z then [x, y, z] 
				else [x, z, y]
			else [z, x, y]
	else if y<z then 
		if x<z then [y, x, z] 
			else [y, z, x] 
		else [z, y, x]

-- Task 5
norm x = sqrt (fst x * fst x + snd x * snd x)
add x y = (fst x + fst y, snd x + snd y)
scalarMult x y = (y*fst x, y*snd x)
dot x y = fst x * fst y + snd x * snd y

-- Task 6
asciiRange fromChar toChar = zip [fromChar..toChar] [ord fromChar..ord toChar]

-- Task 7
incn n string = [chr (ord c + n) | c <- string, c `elem` concat[['a'..'z'], ['A'..'Z']]]