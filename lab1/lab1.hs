{-Rohan Mirchandani CS 115 Lab 1-}

{-Part B-}
infixl 7 +*
(+*) :: Double -> Double -> Double
(+*) x y = x^2 + y^2

infixr 3 ^||
(^||) :: Bool -> Bool -> Bool
(^||) x y | x == False = y
          | otherwise = y == False

rangeProduct :: Integer -> Integer -> Integer
rangeProduct a b | a < b = a * rangeProduct (a + 1) b
                 | a == b = b
                 | otherwise = error "Second number less than first"


prod :: [Integer] -> Integer
prod = foldr (*) 1


rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 a b | a <= b = prod [a .. b]
                  | otherwise = error "Second number less than first"



map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = [] -- cases where one list empty
map2 _ _ [] = [] 
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys

map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ [] _ _ = [] -- cases where one list empty
map3 _ _ [] _  = [] 
map3 _ _ _ [] = [] 
map3 f (x:xs) (y:ys) (z:zs) = f x y z : map3 f xs ys zs

{-Dot product evaluation-}

{- 
 Point-free  definition

 dot lst1 lst2
 dot = (\x -> sum . x) . map2
 ((sum .) . map2 (*)) lst1 lst2
 ((sum .) (map2 (*) lst1)) lst2
 ((\x -> sum . x) (map2 (*) lst1)) lst2
 (sum . (map2 (*) lst1)) lst2
 sum (map2 (*) lst1 lst2)






 (sum .) (map2 (*) lst1 lst2)
 (\x -> sum x) (map2 (*) lst1 lst2)
 sum (map2 (*) lst1 lst2)
 

We see that this is the same as evaluating the explicit definiton:
dot lst1 lst2 
sum (map2 (*) lst1 lst2) 
(sum) (lst1[0] * lst2[0] : map2 (*) lst1[1:] lst2[1:])
lst1[0] * lst2[0] : (sum) (map2 (*) lst1[1:] lst2[1:])
...
-}

sumNat = sum [x | x <- [1..999], x `mod` 5 == 0 || x `mod` 3 == 0 ]
-- Result: 233168

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [ n | n <- xs, n `mod` x /= 0 ]

primes = sieve [2..]

sumPrime = sum $ takeWhile (< 10000) primes
{-Result: 5736396-}


{-Part C-}
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (h:t) = h + sumList t


-- You would use pattern matching in the input itself instead of the length. 
-- also use head:tail split instead of head, tail functions.
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [x] = x
largest (x:xs) = max (x) (largest xs)



{-Part D-}

{- fib 3
fib (3-1) + fib (3-2)
fib (2) + fib (3-2)
(fib (2 - 1) + fib (2 - 2))  + fib (3-2)
(fib 1 + fib (2-2)) + fib (3-2)
(1 + fib (2-2)) + fib (3-2)
(1 + fib 0) + fib 3-2
(1 + 0) + fib (3-2)
1 + fib (3-2)
1 + fib 1
1 + 1
2
-}



{- fact 3
fact 3 
3 * fact(3-1)
3 * ((3-1)) * fact ((3-1)-1)
 * fact (3-1-1-1)))
3 * ((3-1) * ((3-1-1) * ((3-1-1-1) * (fact 3-1-1-1-1))))

... this continues indefinitely
 This definition is wrong because fact n is always checked first for any argument
 so it will never reach the base case of fact 0 = 1. Fix this by matching for
 fact 0 = 1 first, so the recursion can eventually hit the base case.
-}

-- Question: is the reason we dont have to evaulate the input to fact 3
-- because the n matching case comes before the 0 case?

{- reverse [1,2,3]
    iter  [1,2,3] []
    iter [2, 3] (1:[])
    iter [3] 2:(1:[])
    iter [] 3:(2:(1:[]))
    3:(2:(1:[]))
    [3,2,1]

    The time complexity of this function is O(n) because we 
    simply appeand each element (when it is the edge) to our list in iter [],
    then we have n operations to combine the new list.
-}

{- reverse [1,2,3]
(reverse [2,3]) ++ [1]
((reverse [3]) ++ [2]) ++ [1]
((reverse [] ++ [3]) ++ [2]) ++ [1]
(([] ++ [3]) ++ [2]) ++ [1]
([3] ++ [2]) ++ [1]
(3: ([] ++ [2])) ++ [1] 
(3: [2])) ++ [1] 
[3,2] ++ [1] 
3: ([2] ++ [1]) 
3: (2:([] ++ [1]))
3:(2:([1]))
3:[2,1]
[3,2,1]

Ben Bitfiddle was wrong because each ++ operation in itself is O(n) recursive
call that uses : on each element in the input list.  He didn't compute
the ++ redex completely.  Therefore, the total complexity is O(n^2). 


-}

{-
   head (isort [3,1,2,5,4])
   evaluate isort [3,1,2,5,4] first
   insert 3 (isort [1,2,5,4])
   insert 3 (insert 1 isort [2, 5, 4])
   insert 3 (insert 1 (insert 2 isort [5, 4]))
   insert 3 (insert 1 (insert 2 (insert 5 isort [4])))
   insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))
   insert 3 (insert 1 (insert 2 (insert 5 (insert 4 ([])))
   insert 3 (insert 1 (insert 2 (insert 5 [4])))
   insert 3 (insert 1 (insert 2 ( 4: insert 5 [])))
   insert 3 ( 1 : 2 : 4: insert 5 [])))
   1: insert 3 (2 : 4 : insert 5 [])
   --> head 1: insert 3 (2 : 4 : insert 5 [])
   1 

-}

-- Question: i thought we had to evaluate the list inputs to insert because 
-- one of the pattern matching statements matches with [], which requires
-- the list to be known.


{- foldr max 0 [1, 5, 3, -2, 4]
   max 1 (foldr max 0 [5, 3, -2, 4])
   max 1 (max 5 (foldr max 0 [ 3, -2, 4]))
   max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
   max 1 (max 5 (max 3 (max -2 ( foldr max 0 [4]))))
   max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
   max 1 (max 5 (max 3 (max -2 (max 4 0)))))
   max 1 (max 5 (max 3 (max -2 4))))
   max 1 (max 5 (max 3 4))
   max 1 (max 5 4)
   max 1 5
   5
   -}

   {-
     foldl max 0 [1, 5, 3, -2, 4]
     foldl max (max 0 1) [5, 3, -2, 4]
     foldl max (max (max 0 1) 5) [3, -2, 4]
     foldl max (max (max (max 0 1) 5) 3) [-2, 4]
     foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
     foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
     max (max (max (max (max 0 1) 5) 3) -2) 4)
     max (max (max (max 1 5) 3) -2) 4)
     max (max (max 5 3) -2) 4)
     max (max 5 -2) 4)
     max 5 4)
     5

 The space complexity of foldr is the same as foldl because they both evaluate the same
 number of recursive calls to max function at their largest point. Due to lazy evaluation, 
 both only start to reduce once there are 5 max functions within eachother.
 
    -}



