-- 1.1
data Date = Date Int Int Int 
  deriving Show

showDate :: Date -> String
showDate (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y

-- 1.2
data Point = Point Double Double
  deriving Show
data Shape2 = 
    Circle2 Point Double 
  | Rectangle2 Point Point
  deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point x y) (Circle2 (Point x0 y0) r) = Circle2 (Point (x+x0) (y+y0)) r 
translate (Point x y) (Rectangle2 (Point x1 y1) (Point x2 y2)) = Rectangle2 (Point (x1+x) (y1+y)) (Point (x2+x) (y2+y))

-- 1.3
inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point x0 y0) r) (Point x y) = (x - x0)^2 + (y - y0)^2 <= r^2
inShape (Rectangle2 (Point x1 y1) (Point x2 y2)) (Point x y) = abs (x2 - x1) >= x && abs (y2 - y1) >= y   

-- exercise 2.
data Level    = Bachelor | Master | PhD deriving (Show,Eq)

data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double } deriving Show

bestStudent = Student 
  { studentId = "0036491215"
  , firstName = "John", lastName = "Doe"
  , level = Master, avgGrade = 5.0 }

someStudent = Student 
  { firstName = "Marko", lastName  = "Maric"
  , studentId = "20358", level = Bachelor
  , avgGrade = 3.4 }

-- 2.1
improveStudent :: Student -> Student 
improveStudent s = s { avgGrade = if avgGrade s <= 4.0 then avgGrade s+1 else avgGrade s }

