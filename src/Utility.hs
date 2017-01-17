{-# LANGUAGE TemplateHaskell #-}
module Utility where
import NetList
import GenMuxs
import Data.Int
import Data.Bits
import Control.Monad
import Data.Char (ord)

-- Returns a bit true if n4 (a 4-sized nap) holds the value i
select4_bit :: Int8 -> Var -> VarMonad Var
select4_bit i n4 = do
    b0  <- n4 @: 0
    b1  <- n4 @: 1
    b2  <- n4 @: 2
    b3  <- n4 @: 3
    bn1 <- select2_bit i1 b1 b0
    bn2 <- select2_bit i2 b3 b2
    r   <- bn1 &: bn2
    return r
 where i1 = i `mod` 4
       i2 = (i `div` 4) `mod` 4

-- Returns true is v1v0 holds the value of i (v0 is the LSB)
select2_bit :: Int8 -> Var -> Var -> VarMonad Var
select2_bit i v1 v0 = case ((i `div` 2) `mod` 2,i `mod` 2) of
    (0,0) -> do r0 <- notv v0
                r1 <- notv v1
                r  <- r0 &: r1
                return r
    (0,1) -> do r1 <- notv v1
                r  <- r1 &: v0
                return r
    (1,0) -> do r0 <- notv v0
                r  <- v1 &: r0
                return r
    (1,1) -> do r  <- v0 &: v1
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

adderInt :: Int8 -> Var -> Var -> VarMonad (Var,Var)
adderInt 1 xorxy andxy = do
  xorb <- xorxy @: 0
  andb <- andxy @: 0
  return (andb,xorb)


adderInt n xorxy andxy = do
  xorb <- xorxy @: (n-1)
  andb <- andxy @: (n-1)
  (c,low) <- adderInt (n-1) xorxy andxy
  co' <- c &: xorb
  co <- co' |: andb
  outb <- c ^: xorb
  out <- outb -: low
  return (co,out)

-- An adder of two variables of the same size. The first value returned is the
-- rest of the addition and the second one the sum.
adder :: Var -> Var -> VarMonad (Var,Var)
adder x y =
  if size x /= size y then error "wrong size adder"
  else
  do
  xorxy <- x ^: y
  andxy <- x &: y
  adderInt (size x) xorxy andxy

oneadderInt :: Int8 -> Var -> VarMonad (Var,Var)
oneadderInt 1 x  = do
  xb <- x @: 0
  xor <- notv xb
  return (xb,xor)

oneadderInt n x = do
  xb <- x @: (n-1)
  (c,low) <- oneadderInt (n-1) x
  outb <- xb ^: c
  co <- xb &: c
  out <- outb -: low
  return (co ,out)

-- An adder of a variable and 1. The first value returned is the rest of the
-- addition and the second one the sum.
oneadder :: Var -> VarMonad (Var,Var)
oneadder x = oneadderInt (size x) x


subberInt :: Int8 -> Var -> Var -> VarMonad (Var,Var)
subberInt 1 xorxy andxy = do
  xorb <- xorxy @: 0
  andb <- andxy @: 0
  outb <- notv xorb
  co <- xorb |: andb
  return (co,outb)


subberInt n xorxy andxy = do
  xorb <- xorxy @: (n-1)
  andb <- andxy @: (n-1)
  (c,low) <- subberInt (n-1) xorxy andxy
  co' <- c &: xorb
  co <- co' |: andb
  outb <- c ^: xorb
  out <- outb -: low
  return (co,out)

subber :: Var -> Var -> VarMonad (Var,Var)
subber x y =
  if size x /= size y then error "wrong size subber"
  else
  do
  y' <- notv y
  xorxy <- x ^: y'
  andxy <- x &: y'
  subberInt (size x) xorxy andxy
  

-- Applies a binary operation between a list of variable (assumes the operation
-- is associative).
long_bin :: (Var -> Var -> VarMonad Var) -> [Var] -> VarMonad Var
long_bin b [] = fail "long_bin on empty list"
long_bin b l  = foldM b (head l) $ tail l

long_or  = long_bin (|:)
long_and = long_bin (&:)

-- Applies a binary operation on the bits of a nap (assumes the operation is
-- associative)
nap_bin :: (Var -> Var -> VarMonad Var) -> Var -> VarMonad Var
nap_bin b n = n @: 0 >>= \d -> foldM (\v -> \i -> n @: i >>= \v2 -> b v v2)
                                     d [1 .. (s-1)]
 where s = size n

nap_or = nap_bin (|:)
nap_and = nap_bin (&:)

-- Tests if an input var is the "same" as a number with a given function, and
-- if the result (which must be a bit) yields 1, give the value of the
-- associated variable in the list.
-- The value returned is undefined if their is no match.

longSelect :: Int -> Var -> [(Int8,Var)] -> VarMonad Var
longSelect _ _ [(_,rv)] = return rv
longSelect n v rvs = do
    b <- v @: fromIntegral (n-1)
    let listOne = filter (flip testBit (n-1) . fst) rvs
    let listZero = filter (not . flip testBit (n-1) . fst) rvs
    let recOne = longSelect (n-1) v listOne
    let recZero = longSelect (n-1) v listZero
    if null listOne then recZero
    else if null listZero then recOne
    else do
        varOne <- recOne
        varZero <- recZero
        b <: (varOne,varZero)

long_select2 = longSelect 2
long_select4 = longSelect 4

binaryToIntegral :: (Integral a) => String -> a
binaryToIntegral = fromIntegral . fst . foldr (\c (res, pow2) -> (res + pow2*(ord c - ord '0'), 2*pow2)) (0, 1)

binaryToInt8 :: String -> Int8
binaryToInt8 = binaryToIntegral

binaryToInt :: String -> Int
binaryToInt = binaryToIntegral

$(genMuxs 10)

inputV ::String -> Int8 -> Var
inputV s i = (s,i,Einput)

vconstV :: String -> Int8 -> Int -> Var
vconstV nm s c = (nm, s, Econst c)

(^-:) nap v = do
  n <- notv nap
  v <: (n,nap)

renameV :: String -> Var -> Var
renameV name (_,s,t) = (name,s,t)

renameVM :: String -> VarMonad Var -> VarMonad Var
renameVM name vm = do
  v <- vm
  let res = renameV name v
  return res
