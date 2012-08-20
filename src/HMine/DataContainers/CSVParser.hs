module HMine.DataContainers.CSVParser
    ( loadCSV
    , list2csv
    , module Text.ParserCombinators.Parsec
    )
    where
          
import System.IO
import Text.ParserCombinators.Parsec
  
-- | Loads a CSV file.  Algorithms store debugging info in CSV files, and this function is used by the plotting routines
loadCSV :: String -> IO (Either ParseError [[String]])
loadCSV filename = do
    hin <- openFile filename ReadMode
    str <- hGetContents hin
    let ds = parseCSV str
    return ds
  
-- | Converts a list into CSV format for writing to a file
list2csv :: [String] -> String
list2csv xs = foldl addcommas "" $ map addquotes xs
    where
        addcommas x y = 
            if x==""
               then y
               else x++","++y
        addquotes x = show x
-- list2csv xs = init $ tail $ show xs

-- CSV parser from "Real World Haskell," p. 391
-- modified to allow multiple spaces between cells
    
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
    
csvFile = endBy line eol
line = sepBy cell (char ',' <|> char ' ')
cell = do
    spaces
    quotedCell <|> many (noneOf " ,\n\r")

quotedCell = do
    char '"'
    content <- many quotedChar
    char '"' <?> "Quote at end of cell"
    return content
    
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"
   
-- test csv parsing
{-
csv_test = do
    dm <- loadData $ defDatafileDesc {datafileName="../testdata/ringnorm.data"}
    putStrLn $ func dm
    where
          func (Left x) = show x
          func (Right x) = show $ take 10 x-}
