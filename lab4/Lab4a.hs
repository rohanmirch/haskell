--Rohan Mirchandani
-- CS 115 Lab4
import Data.Char
-- A.1

{-

myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) =
  do putChar c
     myPutStrLn cs

--}
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = putChar c >> myPutStrLn cs


--A.2
-- We just delete the "do" because there is only one command
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

--A.3
 {- Original function:
    greet2 :: IO ()
    greet2 = do
      putStr "Enter your name: "
      name <- getLine
      putStr "Hello, "
      putStr name
      putStrLn "!"-}

-- simple way
greet2 :: IO ()
greet2 = putStr "Enter your name: " >> 
         (getLine >>= \name -> putStr "Hello, " >> putStr name >> putStrLn "!")
 
-- complex way
greet2a :: IO ()
greet2a = putStr "Enter your name: " >> 
        getLine >>= \name -> case name of
            _ ->  putStr "Hello, " >> putStr name >> putStrLn "!"

{- The complex desugaring does not behave differently than the simple 
    desugaring in this case. -}

-- A.4
--import Data.Char
{- Original function: 

greet3 :: IO ()
greet3 = do
  putStr "Enter your name: "
  (n:ns) <- getLine
  let name = toUpper n : ns
  putStr "Hello, "
  putStr name
  putStrLn "!"

-}
-- simple way
greet3 :: IO ()
greet3 = do
  putStr "Enter your name: " >> 
      getLine >>= \(n:ns) ->
      let name = toUpper n : ns in
      putStr "Hello, " >> putStr name >> putStrLn "!"

Complex way
greet3a :: IO ()
greet3a = do
  putStr "Enter your name: " >> 
      getLine >>= \name -> case name of
      (n:ns) -> let name = toUpper n : ns in putStr "Hello, " >> putStr name >> putStrLn "!"
      _ -> error "Pattern match failure in do expression"



{- If we enter a an empty name in the complex desugaring, then we will
    enter the error case. -}

{-Question: why does this dive me a different answer and 
  never hit the error case for an empty string? 

greet3a :: IO ()
greet3a = do
  putStr "Enter your name: " >> 
      getLine >>= \(n:ns)-> case (n:ns) of
      (n:ns) -> let name = toUpper n : ns in putStr "Hello, " >> putStr name >> putStrLn "!"
      _ -> error "Pattern match failure in do expression"

-}




















