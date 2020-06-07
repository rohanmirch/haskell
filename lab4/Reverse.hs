module Main where
import System.Environment 
import System.Exit

main :: IO ()
main = do
        fileArgs <- getArgs
        progName <- getProgName
        

        if (length fileArgs /= 1) then do
            putStrLn ("usage: " ++ progName ++ " " ++ unwords fileArgs)
            exitFailure
        else do

        -- note: use <- for monad and = for regular
        let filename = head fileArgs
        content <- readFile filename
        let allLines = reverse (lines content)
        mapM_ putStrLn allLines
        return ()
        exitSuccess

