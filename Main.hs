
module Main where
import NetList
import Data.Either

fulladder 0 v1 v2 = do
    e1 <- v1 @: 0
    e2 <- v2 @: 0
    s  <- e1 ^: e2
    r  <- e1 &: e2
    return (s,r)
fulladder n v1 v2 = do
    (s',r') <- fulladder (n-1) v1 v2
    e1 <- v1 @: n
    e2 <- v2 @: n
    t1 <- e1 ^: e2
    s  <- t1 ^: r'
    t2 <- t1 &: r'
    t3 <- e1 &: e2
    r  <- t2 |: t3
    ns <- s' -: s
    return (ns,r)

netlist = do
    let n = 8
    v1 <- input "i1" n
    v2 <- input "i2" n
    (r,s) <- fulladder (n-1) v1 v2
    return [r,s]

main :: IO ()
main = if isRight er then let (Right s) = er in putStrLn s
       else let (Left s) = er in fail $ "Error : " ++ s
 where er = writeNetlist netlist

