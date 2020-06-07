-- Rohan Mirchandani
-- Lab 3

module Lab3ab where

--A.2
data Nat = Zero | Succ Nat
    deriving (Eq, Show)

    
-- A.1
{--
natEq :: Nat -> Nat -> Bool
natEq Zero Zero = True
natEq (Succ a) (Succ b) = natEq a b
natEq _ Zero = False
natEq Zero _ = False

instance Eq Nat where
    (==) = natEq
    x /= y = not (x == y)


instance Show Nat where
    show Zero = "Zero"
    show (Succ Zero) = "Succ Zero"
    show (Succ a) = "Succ (" ++ show a ++ ")"
--}

-- A.3
-- just the <= function

natOrd :: Nat -> Nat -> Bool
natOrd Zero Zero = True
natOrd (Succ a) (Succ b) = natOrd a b
natOrd Zero _ = True
natOrd _ Zero = False

instance Ord Nat where
    (<=) = natOrd


{-- 
    This would work because haskell first orders Zero < Succ Nat,
    which is true. It would then just recursively compares the Nat
    inside the Succ until it reaches some Zero (which is the original inequality).
 --}

data SignedNat =
    Neg Nat | Pos Nat
    deriving (Show)

signedEq :: SignedNat -> SignedNat -> Bool
signedEq (Neg Zero) (Pos Zero) = True
signedEq (Pos Zero) (Neg Zero) = True
signedEq (Neg a) (Neg b) = a == b
signedEq (Pos a) (Pos b) = a == b
signedEq _ _ = False

instance Eq SignedNat where
    (==) = signedEq
    -- x /= y = not (x == y)

signedOrd :: SignedNat -> SignedNat -> Bool 
signedOrd (Neg Zero) (Pos Zero) = True
signedOrd (Pos Zero) (Neg Zero) = True
signedOrd (Pos _) (Neg _) = False
signedOrd (Neg _) (Pos _) = True
signedOrd (Pos a) (Pos b) = a <= b
signedOrd (Neg a) (Neg b) = a >= b --need a to have more Succs to be more negative

instance Ord SignedNat where
    (<=) = signedOrd

{- We cannot automatically define Eq for SignedNat because haskell cannot 
    differentiate between Neg and Pos, and will evaluate each the same.
    The ord woyldn't work for the same reason, so more negetive numbers would
    be deemed "bigger" than less negative numbers because they have more recursive calls.-}



-- A.5
addNat :: Nat -> Nat -> Nat
addNat Zero a = a
addNat a Zero = a
addNat (Succ a) b = Succ (addNat a b)

signedAdd :: SignedNat -> SignedNat -> SignedNat
signedAdd (Neg Zero) a = a
signedAdd a (Neg Zero) = a
signedAdd (Pos Zero) a = a
signedAdd a (Pos Zero) = a
signedAdd (Pos a) (Pos b) = Pos (addNat a b)
signedAdd (Neg a) (Neg b) = Neg (addNat a b)
-- decrement from each side (pos and neg) until one reaches 0
signedAdd (Neg (Succ a)) (Pos (Succ b)) = signedAdd (Neg a) (Pos b)
signedAdd (Pos (Succ a)) (Neg (Succ b)) = signedAdd (Pos a) (Neg b)

mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat _ Zero = Zero
mulNat (Succ Zero) a = a
mulNat a (Succ Zero) = a
-- add a to a (b times)
mulNat a (Succ b) = addNat a (mulNat a b)

signedMul :: SignedNat -> SignedNat -> SignedNat
signedMul (Pos a) (Pos b) = Pos (mulNat a b)
signedMul (Pos a) (Neg b) = Neg (mulNat a b)
signedMul (Neg a) (Pos b) = Neg (mulNat a b)
signedMul (Neg a) (Neg b) = Pos (mulNat a b)


signedNeg :: SignedNat -> SignedNat
signedNeg (Pos a) = Neg a
signedNeg (Neg a) = Pos a

signedAbs :: SignedNat -> SignedNat
signedAbs (Neg a) = Pos a
signedAbs a = a

-- Represents the "sign" of the number : +- 1
signedSig :: SignedNat -> SignedNat
signedSig (Pos Zero) = Pos Zero
signedSig (Neg Zero) = Neg Zero
signedSig (Pos _) = Pos (Succ Zero)
signedSig (Neg _) = Neg (Succ Zero)


fromInt :: Integer -> Nat
fromInt 0 = Zero
fromInt a = Succ (fromInt (a - 1))

signedFromInt :: Integer -> SignedNat
signedFromInt a | a >= 0 = Pos (fromInt a)
                | otherwise = Neg (fromInt (-a))


instance Num SignedNat where
    (+) = signedAdd
    (*) = signedMul
    negate = signedNeg
    abs = signedAbs
    signum = signedSig
    fromInteger = signedFromInt



-- A.6
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ a) = 1 + (natToInteger a)

signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos a) = natToInteger a
signedNatToInteger (Neg a) = (*) (-1) (natToInteger a)


--A.7
{--
    One redundant thing is that Pos Zero and Neg Zero are techically equivalent
    but we have two differetn names so we have to pattern match with seemingly redundant cases.
    We could make  data SignedNat = Neg Nat | Pos Nat | Zero instead. 

    This would allow us just to match on one zero value, while treating positives and negatives 
    separately as we already do.

--}

factorial :: (Num a, Ord a) => a -> a
factorial n | n < 0 = error "negative number"
            | n == 0 = 1
            | otherwise = n * factorial (n-1) 


{-Result: Pos (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))) -}


{--Part B--}

--B.1

{-
>#< should be infix. The result is a string, so it can't be combined with another Integer
because that would produce a type error. Therefore, it can't be right or left assosiative.

+| could be either because last digits of the sum are just integers based on the last digits of the 
   of the added numbers. Therefore, right and left associative evaluation will give the same result.

&< is infixl because we always want to append the right operand to the end of the list. i
    we evaluate right associative, then we may end up trying to append a list to an integer, 
    which will give a type error.

>&& is infixr because we want to prepend the left operand. If we evaluate left assoicativity,
    then we might be trying to prepend a list to a list, which is a type error.
-}

--B.2
{-

Since we are simply adding integers and getting an integer, it can be Infix, Infixr or infixl.
It should be infix becuase the answer can change depending on if you do
left or right associative evaluation.


-}





































