module Main where
import System.Environment 
import System.Exit
import Data.Char
import Data.List
import System.IO




main :: IO ()
main = do
        fileArgs <- getArgs
        progName <- getProgName
        -- check for valid number and type of columns
        if (length fileArgs < 2) || checkCols (init fileArgs) == False then do
                putStrLn ("usage: " ++ progName ++ " " ++ (unwords fileArgs))
                exitFailure
        else do
                let filename = last fileArgs
                let colList = getCols fileArgs
                -- check for all columns > 0
                if (all ((<=) 1) colList) then do
                        -- take file as our lines
                        if filename /= "-" then do     
                                content <- readFile filename
                                let allLines = lines content
                                printer allLines colList
                        -- take stdin as our lines       
                        else do 
                                content <- hGetContents stdin
                                let allLines = lines content
                                printer allLines colList
                        
                
                else do 
                        putStrLn ("usage: " ++ progName ++ " " ++ (unwords fileArgs))
                        exitFailure

        exitSuccess


-- checks if all the column arguments are numbers
checkCols :: [String] -> Bool
checkCols [] = True
checkCols (x:xs) = (all isDigit x) && (checkCols xs)


-- maps all arguments to columns
getCols :: [String] -> [Int]
getCols [] = []
getCols lst = map (\x -> read x) (init lst)

-- creates a new list of words based on the given column numbers
getWords :: [String] -> [Int] -> [String]
getWords _ [] = []
getWords lst (colNum:rest) | colNum < (length lst) +1 = lst!!(colNum-1) : (getWords lst rest)
                          | otherwise = getWords lst rest

-- takes in a line of words, get the filtered list, and prints it as a new line
printLine :: String -> [Int] -> IO ()
printLine line cols = do
        let newLine = intercalate (" ") (getWords (words line) cols)
        putStrLn (newLine)
        --return ()

-- Prints all lines
printer :: [String] -> [Int] -> IO ()
printer [] _ = return ()
printer (x:xs) cols = do
        printLine x cols
        printer xs cols













