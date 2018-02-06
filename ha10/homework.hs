import Control.Applicative
import Data.List (isInfixOf, intercalate, (\\))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as Tio
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory
import System.Environment (getArgs, withArgs)

-- time :: IO () -> IO Double 
time f = do 

-- print str >> getPOSIXTime >>= (- getPOSIXTime) 
 
  x <- getPOSIXTime
  f
  y <- getPOSIXTime
  return $ y - x
     
-- |Like UNIX grep command
grep :: String -> FilePath -> IO ()
grep xs f = do
  allLines <- loadFile f 

  mapM_ putStrLn $ filter (isInfixOf xs) allLines

-- |Loads file from given path and returns IO [String]
loadFile :: FilePath -> IO [String]
loadFile f = lines <$> readFile f 

-- |Expects arguments in form: [stringForFilter, filePath]
grepWithArgs :: IO ()
grepWithArgs = do 
  (xs:f:_)  <- getArgs 
  grep xs f

-- |Like grep, but uses Data.Text so it's more efficient
grepText :: T.Text -> FilePath -> IO ()
grepText xs f = do
  allLines <- loadFile f
  let allLinesText = map T.pack allLines

  mapM_ putStrLn $ map T.unpack $ filter (T.isInfixOf xs) allLinesText
  


-- |Task 3. pseudoDatabase 
type Table = (FilePath, [String])

-- |Creates table with given name + ".tbl" and given column labels.
dbCreateTable :: String -> [String] -> IO ()
dbCreateTable f attri = do
  writeFile (f ++ ".tbl") $ intercalate " " attri 
  

-- |Deletes a file that represents a database 
dbDeleteTable :: Table -> IO ()
dbDeleteTable (f,_) = do
  removeFile f 

-- |Inserts a row into the table 
dbInsert :: Table -> [String] -> IO ()
dbInsert (f,attri) xs 
  | length attri /= length xs = error "Your entry is invalid."
  | otherwise = do
                appendFile f $ "\n" ++ intercalate " " xs 

-- |Takes a predicate and returns all rows that satisfy it.
dbSelect :: Table -> ([String] -> Bool) -> IO [[String]]  
dbSelect (f,_) filtF = do 
  allLines <- loadFile f 
  let xss = map words allLines

  return $ filter filtF xss

   
-- |Takes a predicate and deletes all rows that satisfy it. 
dbDelete :: Table -> ([String] -> Bool) -> IO ()
dbDelete (f,_) filtF = do 
  allLines <- loadFile f
  let xss = map words allLines
  
  dbDeleteTable (f,[]) 
  writeFile f $ unlines $ map unwords $ xss \\ filter filtF xss 

-- |Prints the table to standart input. 
dbPrintTable :: Table -> IO ()
dbPrintTable (f,_) = do 
  all <- dbSelect (f,[]) (const True) 
  mapM_ putStrLn $ map unwords all



