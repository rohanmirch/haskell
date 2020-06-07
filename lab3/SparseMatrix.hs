-- Rohan Mirchandani
-- Lab3

module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)



-- Checks the indices against the bounds
checkBounds :: [((Integer, Integer), a)] -> (Integer, Integer) -> Bool
checkBounds [] _ = True
checkBounds (((row, col), _):t) (b1, b2)= (row >= 1 && row <= b1) && (col >= 1 && col <= b2) 
                                    && checkBounds t (b1, b2)


sparseMatrix :: (Eq a, Num a) => 
    [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
sparseMatrix lst (b1,b2) | b1 < 1 || b2 < 1 || not (checkBounds lst (b1, b2)) = error "bounds error"
                         | otherwise  = 
                           let m = M.filter (/= 0) (M.fromList lst) in
                           let keys = M.keys m in
 SM {bounds = (b1, b2),
    rowIndices = (S.fromList (map (fst) keys)),
    colIndices = (S.fromList (map (snd) keys)), 
    vals = m}

-- we will just turn the additive union into a list, then call our sparseMatrix constructor function
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM bounds1 _ _ m1) (SM bounds2 _ _ m2 ) = if (bounds1 /= bounds2) then error "bounds not same"
        else sparseMatrix (M.toList (M.unionWith (+) m1 m2)) bounds1
 
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM b rows cols m) = SM b rows cols (M.map (negate) m)

subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM m1 m2 = addSM m1 (negateSM m2)


-- C.5
-- given two maps and row/column values, computes the product of the row and column
multVec :: (Eq a, Num a) => (M.Map (Integer, Integer) a) -> 
            (M.Map (Integer, Integer) a) -> Integer -> Integer -> a
multVec m1 m2 r c = 
    -- get submaps of just row and column, with just relevant index as tuple
    let row = M.mapKeys (snd) (M.filterWithKey (\k _ -> (fst k) == r) m1) in
    let col = M.mapKeys (fst) (M.filterWithKey (\k _ -> (snd k) == c) m2) in
    -- multiply them together and compute the sum
    M.foldl (+) 0 (M.intersectionWith (*) row col)
    


-- given a row value in m1, creates all entries by multiplying by all columns in m2 and
-- creating list of (key, value) entries
multRow :: (Eq a, Num a) => (M.Map (Integer, Integer) a) -> (M.Map (Integer, Integer) a) 
        -> Integer -> [Integer] -> [((Integer, Integer), a)]
multRow _ _ _ [] = []
multRow m1 m2 rowNum (colNum:rest) =  ((rowNum,colNum), multVec m1 m2 rowNum colNum) : (multRow m1 m2 rowNum rest)
    


mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM (b1, b2) r1 _ m1) (SM (b3, b4) _ c2 m2) | b2 /= b3 = error "bounds error"
    | otherwise = 
    -- for each row, compute entries (for each column), return as nested list of key val pairs
    let rowList = S.toList r1 in
    let colList = S.toList c2 in
    let keyValList1 = map (\x -> (multRow m1 m2 x colList)) rowList in
    -- combine nested list into one full list of key-val entries
    let keyValList = foldr (++) [] keyValList1 in
    sparseMatrix keyValList (b1, b4)


-- C.6
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM (b1, b2) _ _ m) (k1, k2) | (k1 <= b1 && k1 > 0 && k2 <= b2 && k2 > 0) = 
                                        M.findWithDefault 0 (k1, k2) m
                                  | otherwise = error "bounds error"

rowsSM :: (Eq a, Num a) => SparseMatrix a -> Integer
rowsSM (SM (b1, _) _ _ _) = b1

colsSM :: (Eq a, Num a) => SparseMatrix a -> Integer
colsSM (SM (_, b2) _ _ _) = b2

                                                    
-- C.7
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) = getSM



-- C.8
{--
    We don't have a clear way of converting sparse matrices to actual numbers.
    This would make it hard to represent as Integers and so forth, which is 
    what we need for Num type class.
--}




