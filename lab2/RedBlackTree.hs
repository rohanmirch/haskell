{-Rohan Mirchandani CS 115 Lab 2-}
module RedBlackTree where

-- A color is either red or black.
data Color = Red | Black
  deriving Show

-- A red-black tree is either a leaf or a tree node with a color,
-- two branches, both of which are trees, and a value of type a.
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
  deriving Show


{-Part A-}
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node _ left val right) | x == val = True
                                 | x < val = member x left
                                 | otherwise = member x right


toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ left val right) = toList left ++ [val] ++ toList right


insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t)
  where
    -- Insert an element into a tree.
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf  -- new nodes are colored red
    ins elem t@(Node color left elem' right)
      | elem < elem' = balance color (ins elem left) elem' right
      | elem > elem' = balance color left elem' (ins elem right)
      | otherwise = t  -- element already in the tree; no insertion required

    -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- invariants 2 and 3.
    balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    balance Black (Node Red (Node Red l1 e1 r1) e2 r2) e t =
      Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
    
    balance Black (Node Red a x (Node Red b y c)) z d = 
        Node Red (Node Black a x b) y (Node Black c z d)

    balance Black a x (Node Red (Node Red b y c) z d) = 
        Node Red (Node Black a x b) y (Node Black c z d)

    balance Black a x (Node Red b y (Node Red c z d)) = 
        Node Red (Node Black a x b) y (Node Black c z d)

    balance color l e r = Node color l e r  -- no balancing needed



fromList :: Ord a => [a] -> Tree a
fromList lst = foldr (insert) Leaf lst

minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node _ left _ right) = min ((minDepth left) + 1) ((minDepth right) + 1)


maxDepth :: Tree a -> Int
maxDepth Leaf = 0
maxDepth (Node _ left _ right) = max ((maxDepth left) + 1) ((maxDepth right) + 1)

------------- HERE --------------------------
-- I changed this to use the maybe monad
maxVal :: Ord a => Tree a -> Maybe a
maxVal Leaf = Nothing
maxVal (Node _ _ _ right@(Node _ _ _ _ )) = maxVal right
maxVal (Node _ _ val Leaf) = Just val

minVal :: Ord a => Tree a -> Maybe a
minVal Leaf = Nothing
minVal (Node _ left@(Node _ _ _ _) _ _) = minVal left
minVal (Node _ Leaf val _) = Just val

-- I just remembered that we can use error for the Nothing pattern match
clean :: Maybe a -> a
clean (Just val) = val
clean Nothing = error "invalid subtree for maxVal/minVal"


-- in my testInvariant1 function, I use clean to unwrap the Just/Nothing from my max/min functions
testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 Leaf = True
testInvariant1 (Node _ left@(Node _ _ _ _ ) val right@(Node _ _ _ _ )) = (val > clean (maxVal left)) && val < clean (minVal right) 
                                              && testInvariant1 left && testInvariant1 right
testInvariant1 (Node _ left@(Node _ _ _ _ ) val Leaf) = val > clean (maxVal left) &&  testInvariant1 left 
testInvariant1 (Node _ Leaf val right@(Node _ _ _ _ )) = val < clean (minVal right) && testInvariant1 right
testInvariant1 (Node _ Leaf _ Leaf) = True



testInvariant2 :: Tree a -> Bool
testInvariant2 Leaf = True
-- RED node cases: if either of the children are red, false
testInvariant2 (Node Red (Node Red _ _ _ ) _ _) = False
testInvariant2 (Node Red _ _ (Node Red _ _ _ )) = False
-- otherwise, recurse on subtrees
testInvariant2 (Node _ left _ right) = testInvariant2 left && testInvariant2 right


-- NOTE: I can't figure out what is wrong with this function
testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
  where
    -- Given a tree, return a list of the count of black nodes on every path
    -- from the root of the tree to a leaf.
    leafCounts :: Tree a -> Int -> [Int]
    leafCounts Leaf n = [n]
    leafCounts (Node Black left _ right) n = leafCounts left (n + 1) ++ leafCounts right (n + 1)
    leafCounts (Node Red left _ right) n = leafCounts left n ++ leafCounts right n

    -- Return True if all the elements of a list are equal.
    allEqual :: Ord a => [a] -> Bool
    allEqual [] = True
    allEqual [_] = True
    allEqual (x:r@(y:_)) | x == y = allEqual r
                         | otherwise = False



-- We define Set as a type synonym for Tree.
type Set a = Tree a

-- Empty set.
empty :: Set a
empty = Leaf

-- Convert a list to a set.
toSet :: Ord a => [a] -> Set a
toSet = fromList


isSubset :: Ord a => Set a -> Set a -> Bool
-- Check if all members of s1 are in s2
isSubset s1 s2 = all (\x -> member x s2) (toList s1)


eqSet :: Ord a => Set a -> Set a -> Bool
eqSet s1 s2 = (isSubset s1 s2) && (isSubset s2 s1)

union :: Ord a => Set a -> Set a -> Set a
-- Go though and insert all elements from s1 into s2
union s1 s2 = foldr (\x r -> insert x r) s2 (toList s1)


intersection :: Ord a => Set a -> Set a -> Set a
-- Go though s1 and put all elements also in s2 in empty set
intersection s1 s2 = foldr (\x r -> if (member x s2) then (insert x r) else r) empty (toList s1)


difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldr (\x r -> if (member x s2) then r else (insert x r)) empty (toList s1)

















