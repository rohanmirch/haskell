-- Rohan Mirchandani
-- CS 115 Lab 5

import Control.Monad
--import Data.List
-- A.1
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions = do 
    i <- [1..]
    j <- [1 .. i -1]
    k <- [1 .. j - 1]
    l <- [1 .. k - 1]
    guard $ i^3 + l^3 == j^3 + k^3
    return ((i,l), (j, k), i^3 + l^3)


-- A.2
-- using guard
sumNat = sum (do
    x <- [1..999]
    guard $ x `mod` 5 == 0 || x `mod` 3 == 0
    return x)
-- using mzero
sumNat2 = sum (do 
    x <- [1..999]
    if x `mod` 5 == 0 || x `mod` 3 == 0 then return 0 else mzero
    return x)

--A.3
isPalindrome :: Integer -> Bool
isPalindrome n = reverse (show n) == show n

largestPalindrome = maximum (do 
    x <- [100 .. 999]
    y <- [100 .. 999]
    let n = x * y
    guard $ (isPalindrome n)
    return n)
    
-- Answer: 906609

-- A.4

type Expr = [Item]

data Item = N Int | O Op
  deriving Show

data Op = Add | Sub | Cat
  deriving Show

ops :: [Item]
ops = [O Add, O Sub, O Cat]

exprs = do
    o1 <- ops
    o2 <- ops
    o3 <- ops
    o4 <- ops
    o5 <- ops
    o6 <- ops
    o7 <- ops
    o8 <- ops
    return ([N 1, o1, N 2, o2, N 3, o3, N 4, o4, N 5, o5, N 6, o6, N 7, o7, N 8, o8, N 9])

normalize :: Expr -> Expr
normalize [N i] = [N i]
normalize (N i : O Cat : N j : xs) = normalize (N (read (show i ++ show j)::Int) : xs)
normalize (N i : O o : N j : xs) = N i : O o : (normalize ((N j) : xs))
normalize _ = error "bad normalize expression"


evaluate :: Expr -> Int
evaluate [] = 0
evaluate [N n1] = n1
evaluate (N n1 : O Add : N n2 : xs) = (n1 + n2) + (evaluate xs)
evaluate (N n1 : O Sub : N n2 : xs) = (n1 - n2) + (evaluate xs)
evaluate (O Add : N n1 : xs) = (n1) + (evaluate xs)
evaluate (O Sub : N n1 : xs) = (-n1) + (evaluate xs)
evaluate _ = error "bad exprs"


-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs



-- Part B
-- B.1

{- We have the expression
do n1 <- [1..6]
   n2 <- [1..6]
   []
   return (n1, n2)


[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> return (n1, n2)
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >>= \_ -> [(n1, n2)]
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concatMap (\_ -> [(n1, n2)]) []

We are calling concatMap on [], which will return [].
-}


-- B.2

{-
do n1 <- [1..6]
   n2 <- [1..6]
   return <anything>
   return (n1, n2)

[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >> return (n1, n2)
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [return <anything>] >>= \_ -> [(n1, n2)]
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concatMap (\_ -> [(n1, n2)]) [<anything>]
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [(n1, n2)]

Since concatMap's map fucntion doesn't care about what is in the list,
we just map \_ -> [(n1, n2)] regardless of what is in [<anything>]/

Clearly, the second expression deguars to 
[1..6] >>= \n1 -> [1..6] >>= n2 -> return (n1, n2)
[1..6] >>= \n1 -> [1..6] >>= n2 -> [(n1, n2)]
which is the same.

-}


-- B.3

{-

let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
  do ['a', 'a', c1, c2, 'b', 'b'] <- s
     return [c1, c2]

s >>= 
    \y -> case y of
        ['a', 'a', c1, c2, 'b', 'b'] -> return (c1, c2)
    _ -> fail "Pattern match failure in do expression"

s >>= 
    \y -> case y of
        ['a', 'a', c1, c2, 'b', 'b'] -> [(c1, c2)]
    _ -> fail "Pattern match failure in do expression"


We see that we pattern match s on ['a', 'a', c1, c2, 'b', 'b'],
so the first two must be 'a' and the last two must be 'b', If  that's the case
we return the middle two characters. If s doesn;t match this pattern
we just return a fail, which is the empty string. If the fail was an error,
then the expression wouldn't evaluate fully because it would stop when
it hits the error. Since fail is a list, we can continue our evaluation without problems.

-}

{-
Set m = [x1, x2, ... ]:

foldr ((++) . k) [] [x1, x2, ...]
foldr (\x y -> (++) (k x) y) [] [x1, x2, ...]
(k x1) ++ (k x2) ++ .. ++ []
[k x1, k x2, ...]

concat (map k [x1, x2, ...])
concat [(k x1), (k x2), ...]
(k x1) ++ (k x2) ++ .. ++ []
[k x1, k x2, ...]


Set m = []:

foldr ((++) . k) [] []
foldr (\x y -> (++) (k x) y) [] []
[]

concat (map k [])
concat []
[]


-}



-- B.5
{-
n and s are not the same type. We should try to fix this by casting
them to the same type before we apply (+) operator.
-}























