
module Utility where
import NetList
import Data.Int

-- Returns a bit true if n4 (a 4-sized nap) holds the value i
select4_bit :: Int8 -> Var -> VarMonad Var
select4_bit i n4 = do
    b1  <- n4 @: 0
    b2  <- n4 @: 1
    b3  <- n4 @: 2
    b4  <- n4 @: 3
    bn1 <- select2_bit i1 b1 b2
    bn2 <- select2_bit i2 b3 b4
    r   <- bn1 &: bn2
    return r
 where i1 = i `mod` 4
       i2 = (i `div` 4) `mod` 4

-- Returns true is (v1,v2) holds the value of i (v1 is the LSB)
select2_bit :: Int8 -> Var -> Var -> VarMonad Var
select2_bit i v2 v1 = case (i `mod` 2,(i `div` 2) `mod` 2) of
    (0,0) -> do r1 <- notv v1
                r2 <- notv v2
                r  <- r1 &: r2
                return r
    (0,1) -> do r1 <- notv v1
                r  <- r1 &: v2
                return r
    (1,0) -> do r2 <- notv v2
                r  <- v1 &: r2
                return r
    (1,1) -> do r  <- v1 &: v2
                return r

-- Simple adder between three bits
simple_adder :: Var -> Var -> Var -> VarMonad (Var, Var)
simple_adder c x y = do
    r' <- c ^: x
    r  <- r' ^: y
    c'   <- x &: y
    c''  <- x |: y
    c''' <- c &: c''
    rc   <- c' |: c'''
    return (r,rc)

full_adder :: Int8 -> Var -> Var -> VarMonad (Var,Var)
full_adder 1 x y = do
    x' <- x @: 0
    y' <- y @: 0
    c  <- constV 1 0
    simple_adder c x' y'
full_adder n x y = do
    x' <- x @: (n - 1)
    y' <- y @: (n - 1)
    (r',c') <- full_adder (n-1) x y
    (r,c)   <- simple_adder c' x' y'
    res <- r -: r'
    return (res,c)

