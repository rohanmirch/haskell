-- Rohan Mirchandani
-- CS 115 Lab 5

import Control.Monad
import Data.List
-- A.1
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions = do 
	i <- [1..]
	j <- [1 .. i -1]
	k <- [1 .. j - 1]
	l <- [1 .. k - 1]
	guard $ i^3 + l^3 == j^3 + k^3
	return ((i,l), (j, k))