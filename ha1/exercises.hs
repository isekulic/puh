import Data.Char

-- Exercise 1.1
concat3 str1 str2 str3 = if length str2 < 2 then str1 ++ str3 else str1 ++ str2 ++ str3

-- Exercise 1.2
showSalary amount bonus = if amount < 0 then "Wow, that's messed up!" else 
  if bonus /= 0 then "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus else "Salary is " ++ show amount

