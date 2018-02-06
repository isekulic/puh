type Separator = String
type Document = String
type CSV = [Entry]
type Entry = [Field]
type Field = String

parseCSV :: Separator -> Document -> CSV
parseCSV separ docu = map words (lines $ swapWithSpace (separ!!0) docu)

swapWithSpace :: Char -> String -> String
swapWithSpace deli xs = [ if c == deli then ' ' else c 
  | c <- xs ] 

-- 8.b
--showCSV :: Separator -> CSV -> Document
--showCSV separ csvD = 

-- 8.c
colFields :: Int -> CSV -> [Field]
colFields n csvD 
  | n >= length csvD = error "There is no column n in the CSV document"
  | otherwise = [ c!!n | c <- csvD ]

-- sorry, no time to finish :(