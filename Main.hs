
module Main where
import NetList
import Data.Either
import Data.Bits (shiftL)
import Data.Int
import Debug.Trace
import Control.Monad
-- On Mux assumes 1 -> first choice

netlist = do
    rcmd <- input "rcmd" 4
    wcmd <- input "wcmd" 4
    wval <- input "wval" 16
    we   <- input "we"   1
    low  <- constV 16 0
    lowe <- constV 1  0
    hiw  <- constV 16 0
    hiwe <- constV 1  0
    spw  <- constV 16 0
    spwe <- constV 1  0
    (rr,rw,_,_,_) <- register_manager rcmd wcmd wval we
                                      low lowe hiw hiwe spw spwe
    return [rr,rw]

main :: IO ()
main = if isRight er then let (Right s) = er in putStrLn s
       else let (Left s) = er in fail $ "Error : " ++ s
 where er = writeNetlist netlist

-------------------------------------------------------------------------------
------------------------------ Registers --------------------------------------
-------------------------------------------------------------------------------

-- Returns a bit true if n4 (a 4-sized nap) holds the value i
select_bit i n4 = do
    b1  <- n4 @: 0
    b2  <- n4 @: 1
    b3  <- n4 @: 2
    b4  <- n4 @: 3
    bn1 <- bind2 x1 x2 b1 b2
    bn2 <- bind2 x3 x4 b3 b4
    r   <- bn1 &: bn2
    return r
 where x1 = i `mod` 2
       x2 = (i `div` 2) `mod` 2
       x3 = (i `div` 4) `mod` 2
       x4 = (i `div` 8) `mod` 2
       bind2 x1 x2 v1 v2 = case (x1,x2) of
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

-- Creates a register. Takes a w nap and a write-enable bit. Returns a variable
-- holding the value of the register (before the write)
--    __
-- w-|  |     _
--   |we|>-t-|_|-\
-- /-|__|        |-r
-- |             |
-- \-------------/
--
-- For circular dependencies the DSL is not enough, the variable must be
-- manually created
make_register w we = do
    t <- nlabel
    r <- nlabel
    return $ make t r
 where make t r = rr
        where rt = (t,16,Emux we w rr)
              rr = (r,16,Ereg rt)

-- The register manager. Returns readrreg, readwreg, and the direct access
-- registers.
-- The direct access registers are in order of their numbers, ie Hi, Lo and sp
-- Thus the returned value is : (readrreg, readwreg, hi, lo, sp)
-- When contradictory write orders are given on direct access registers, the
-- direct access one is selected.
register_manager readcmd writecmd writewreg we
                 hiw hiwe low lowe spw spwe
 = do rs <- mapM make_r reg
      readrreg <- dicho_select readcmd  rs 0 3
      readwreg <- dicho_select writecmd rs 0 3
      return (readrreg, readwreg, rs !! 1, rs !! 2, rs !! 7)
 where reg = [( 0, Nothing,  Nothing),   -- ret
              ( 1, Just hiw, Just hiwe), -- hi
              ( 2, Just low, Just lowe), -- lo
              ( 3, Nothing,  Nothing),   -- a0
              ( 4, Nothing,  Nothing),   -- a1
              ( 5, Nothing,  Nothing),   -- a2
              ( 6, Nothing,  Nothing),   -- a3
              ( 7, Just spw, Just spwe), -- sp
              ( 8, Nothing,  Nothing),   -- fp
              ( 9, Nothing,  Nothing),   -- r0
              (10, Nothing,  Nothing),   -- r1
              (11, Nothing,  Nothing),   -- r2
              (12, Nothing,  Nothing),   -- r3
              (13, Nothing,  Nothing),   -- r4
              (14, Nothing,  Nothing),   -- r5
              (15, Nothing,  Nothing)]   -- r6
       make_we (i, mwe) = do
           s1 <- select_bit i writecmd
           s2 <- we &: s1
           case mwe of
               (Just we') -> we' |: s2
               Nothing    -> return s2
       make_w (i, Just w,  Just we') = we' <: (w, writewreg)
       make_w (i, Nothing, Nothing)  = return writewreg
       make_r (i,mw,mwe) = do
           w  <- make_w (i,mw,mwe)
           we <- make_we (i,mwe)
           make_register w we
       mshift :: Int -> Int8 -> Int
       mshift x i = shiftL x $ fromIntegral i
       dicho_select :: Var -> [Var] -> Int -> Int8 -> VarMonad Var
       dicho_select s l x i =
           if i == 0 then do
               sl <- s @: 0
               sl <: (l!!(x+1),l!!x)
           else do
               n1 <- dicho_select s l x $ i - 1
               n2 <- dicho_select s l (x + 1 `mshift` i) $ i - 1
               sl <- s @: i
               sl <: (n2,n1)

