
-- Rohan Mirchandani Lab6
import Control.Monad
import Control.Monad.State
import Data.IORef

-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO ()
whileIO test block =
  do b <- test
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body =
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)


--A.1
factIO :: Integer -> IO Integer
factIO n = 
    if n < 0 then error "argument should be positive"
    else do 
    p <- newIORef 1
    f <- newIORef n
    whileIO 
        (do l <- readIORef f
            return (l > 1)
        )
        (do p' <- readIORef p
            f' <- readIORef f
            writeIORef p (f' * p')
            writeIORef f (f' - 1)
        )

    readIORef p




--A.2
factState :: Integer -> Integer
factState n | n < 0 = error "argument must be positive"
factState n = evalState compute (n, 1)
    where 
        compute :: State (Integer, Integer) Integer
        compute = do
            whileState (\(f, _) -> f > 1)
                (do (f, p) <- get 
                    put (f - 1, f * p)
                )
            (_, p) <- get
            return p


--A.3
fibIO :: Integer -> IO Integer
fibIO n | n < 0 = error "argument must be positive"
fibIO 0 = return 0
fibIO n = do
    a <- newIORef 0
    b <- newIORef 1
    f <- newIORef n
    whileIO
        (do f' <- readIORef f
            return (f' > 1))
        (do 
            a' <- readIORef a
            b' <- readIORef b
            f' <- readIORef f
            writeIORef b (a' + b')
            writeIORef a b'
            writeIORef f (f' - 1)
        )    

    readIORef b


-- A.4
fibState :: Integer -> Integer
fibState n | n < 0 = error "argument must be positive"
fibState 0 = 0
fibState n = evalState compute (0, 1, n)
    where 
        compute :: State (Integer, Integer, Integer) Integer
        compute = do
            whileState (\(_, _, f) -> f > 1)
                (do (a, b, f) <- get 
                    put (b, b + a, f - 1)
                )
            (_, b, _) <- get
            return b




-- Part B

{-


data Reader r b = Reader (r -> b)
runReader :: Reader r a -> r -> a
runReader (Reader f) = f

instance Monad (Reader r) where
  return x = Reader (\r -> x)

  mx >>= f = Reader (\r ->
               let x = runReader mx r in
                 runReader (f x) r)


We will now define f, g, h in terms of their mondayc type and desugar them.

f :: a -> Reader r b
f' :: (a, r) -> b
f x = Reader (\r -> f (x, r))

g :: b -> Reader r c
g' :: (b, r) -> c
g x = Reader (\r -> g (x, r))

h :: a -> Reader r c
h' (x, r) = 
    let y = (f' (x, r)) in
    let z = (g' (y, r)) in
    (z, r)
h' x = Reader (\r -> 
    let y = (f x) r in
    let x = (g y) r in 
    (z r))


f'' :: a -> r -> b
f'' x r = f' (x, r)

g'' :: b -> r -> c
g'' x r = g' (x, r)

h'' :: a -> r -> c
h'' x r = h' (x, r)


We now have:

h = f >=> g
h x = f x >>= g
reversing gives:
f x >>= g = h x

This gives us:
f x >>= g = State (\st -> h' (x, st))

Expanding h gives:
f x >>= g = Reader (\r -> let y = (f x) r in 
        let z = (g y) r in 
        (z r))

Substituting mx in for f x gives:
mx >>= g = Reader (\r -> let y = mx r in 
        let z = (g y) r in 
        (z r))

Using the definition of RunReader, this becomes:

mx >>= g = Reader (\r -> let y = (runReader mx r) in 
                    runReader (g y) r)

mx >>= f = Reader (\r -> let x = (runReader mx r) in 
                    runReader (f x) r)

which is the definition of the >>= operator.


Now we will derive return.

return' (x, r) = x
return' x = (\r -> x)
return x = Reader (\r -> x)














-}














































    