-- Rohan Mirchandani
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1,1)


    where
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
    iter :: Sudoku -> (Int, Int) -> IO Bool
    iter board (row, col) 
        | row <= 9 = do
            possibleVals <- getOKValues board (row, col)
            let nextCol = if col == 9 then 1 else col + 1
            let nextRow = if col == 9 then row + 1 else row
                        -- if its not filled then fill it using recursive helper function
            current <- readArray board (row, col)
            if current == 0 then
                iter' board (row, col) possibleVals
            else do
              -- otherwise, go on to the next square
              iter board (nextRow, nextCol)
        | otherwise = return True


    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
    iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
    iter' _ _ [] = return False
    iter' board (row, col) (x:xs) = do
        -- write value into current square
        writeArray board (row, col) x
        -- get the next value to check in recursion
        let nextCol = if col == 9 then 1 else col + 1
        let nextRow = if col == 9 then row + 1 else row
        -- if board is unsolvable for this value
        solvable <- iter board (nextRow, nextCol)
        if not solvable then do
           -- unmake the move
           writeArray board (row, col) 0
           -- try next valid value
           iter' board (row, col) xs
        else do
           return True




    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
    getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
    getOKValues board (row, col) = do
      rowVals <- getRow board row
      colVals <- getCol board col
      boxVals <- getBox board (row, col)
      return ([1..9] \\ (rowVals `union` colVals `union` boxVals))
      


    -- -- Return the ith row isn a Sudoku board as a list of Ints.
    getRow :: Sudoku -> Int -> IO [Int]
    getRow board row = do
        lst <- mapM (\col -> readArray board (row, col)) [1..9]
        return (filter (> 0) lst)
    


    -- Return the ith column in a Sudoku board as a list of Ints.
    getCol :: Sudoku -> Int -> IO [Int]
    getCol board col = do
        lst <- mapM (\row -> readArray board (row, col)) [1..9]
        return (filter (\x -> x > 0) lst)


    --QUESTION: Why can't I have it like this? It says i get an type error
    --            on the type of value in list being IO [m10 Int] instead of IO [Int]
    {-
    getCol :: Sudoku -> Int -> IO [Int]
    getCol board col = do
        let lst = map (\row -> readArray board (row, col)) [1..9]
        return (filter (\x -> x > 0) lst)
     -}


    -- Return the box covering location (i, j) as a list of Ints.
    getBox :: Sudoku -> (Int, Int) -> IO [Int]
    getBox board (row, col) = do 
    -- get top left corner of box
    let i = 3 * ((row - 1) `div` 3) +1
    let j = 3 * ((col - 1) `div` 3) + 1

    l1 <- readArray board (i, j)
    l2 <- readArray board (i+1, j)
    l3 <- readArray board (i+2, j)
    l4 <- readArray board (i, j+1)
    l5 <- readArray board (i+1, j+1)
    l6 <- readArray board (i+2, j+1)
    l7 <- readArray board (i, j+2)
    l8 <- readArray board (i+1, j+2)
    l9 <- readArray board (i+2, j+2) 
    let l = [l1, l2, l3, l4, l5, l6, l7, l8, l9]

    return (filter (\x -> x > 0) l) 

-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure
