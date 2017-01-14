{-# LANGUAGE TemplateHaskell #-}
module Utility where
import NetList
import GenMuxs
import Data.Int
import Control.Monad
import Data.Char (ord)

-- Returns a bit true if n4 (a 4-sized nap) holds the value i
select4_bit :: Int8 -> Var -> Var
select4_bit i n4 = r
 where b0 = n4 @: 0
       b1  = n4 @: 1
       b2  = n4 @: 2
       b3  = n4 @: 3
       bn1 = select2_bit i1 b1 b0
       bn2 = select2_bit i2 b3 b2
       r   = bn1 &: bn2
       i1 = i `mod` 4
       i2 = (i `div` 4) `mod` 4

-- Returns true is v1v0 holds the value of i (v0 is the LSB)
select2_bit :: Int8 -> Var -> Var -> Var
select2_bit i v1 v0 = case ((i `div` 2) `mod` 2,i `mod` 2) of
    (0,0) -> (notv v0) &: (notv v1)
    (0,1) -> (notv v1) &: v0
    (1,0) -> (notv v0) &: v1
    (1,1) -> v0 &: v1

-- Simple adder between three bits
simple_adder :: Var -> Var -> Var -> (Var, Var)
simple_adder c x y = (r,rc)
 where r'   = c ^: x
       r    = r' ^: y
       c'   = x &: y
       c''  = x |: y
       c''' = c &: c''
       rc   = c' |: c'''

full_adder :: Int8 -> Var -> Var -> (Var,Var)
full_adder 1 x y = simple_adder c x' y'
 where x' = x @: 0
       y' = y @: 0
       c  = constV 1 0
full_adder n x y = (res,c)
 where (r',c') = full_adder (n-1) x y
       (r,c)   = simple_adder c' (x @: (n-1)) (y @: (n-1))
       res     = r -: r'

long_bin :: (Var -> Var -> Var) -> [Var] -> Var
long_bin b [] = error "long_bin on empty list"
long_bin b l  = foldr b (head l) $ tail l

long_or  = long_bin (|:)
long_and = long_bin (&:)

nap_bin :: (Var -> Var -> Var) -> Var -> Var
nap_bin b n = let d = n @: 0 in  foldr (\i -> \v -> let v2 = n @: i in b v v2)
                                      d [1 .. (s-1)]
 where s = size n

nap_or = nap_bin (|:)
nap_and = nap_bin (&:)

-- long_select2 long_select4
long_select :: (Int8 -> Var -> Var) -> Var -> [(Int8,Var)] -> Var
long_select f v [(_,rv)] = rv
long_select f v ((i,rv) : rvs) = b <: (rv,nrv)
 where b   = f i v
       nrv = long_select f v rvs

long_select2 = long_select sel
 where sel i v = select2_bit i (v @: 1) (v @: 0)
long_select4 = long_select select4_bit

binaryToIntegral :: (Integral a) => String -> a
binaryToIntegral = fromIntegral . fst . foldr (\c (res, pow2) -> (res + pow2*(ord c - ord '0'), 2*pow2)) (0, 1)

binaryToInt8 :: String -> Int8
binaryToInt8 = binaryToIntegral

binaryToInt :: String -> Int
binaryToInt = binaryToIntegral

-- $(genMuxs 10)

inputV ::String -> Int8 -> Var
inputV s i = (i,Einput s)

(^-:) nap v = let n = notv nap in  v <: (n,nap)

