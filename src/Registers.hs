
module Registers where
import NetList
import Utility
import Data.Int
import Data.Bits

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
make_register w we name = (rr,rt)
 where rt = (name ++ "_temp",16,Emux we w rr)
       rr = (name,16,Ereg $ name ++ "_temp")

-- The register manager. Takes the following parameters :
--    - readcmd          : the 4-bit name of the register we want to read.
--    - writecmd         : the 4-bit name of the register we want to write to.
--    - writewreg        : the 16-bit value to write in the register.
--    - we               : enable the write.
--    - {hi,lo,sp}{w,we} : 16-bit write and 1-bit write enable for the
--                         registers hi,lo and sp, because they need to
--                         be directly accessed.
-- The returned value is the 16-bit value of the read register, the 16-bit
-- value of the write register (which is thus read-write), and a list of
-- variables which needs to be computed.
-- 
-- When contradictory write orders are given on direct access registers, the
-- direct access one is selected.
register_manager readcmd writecmd writewreg we
                 hiw hiwe low lowe spw spwe
 = runVM (make_gen "register_manager") $ do 
     rs <- mapM make_r reg
     let rs' = map fst rs
     readrreg <- dicho_select readcmd  rs' 0 3
     readwreg <- dicho_select writecmd rs' 0 3
     return (readrreg, readwreg, map snd rs)
 where reg = [( 0, Nothing,  Nothing,   "ret"),
              ( 1, Just hiw, Just hiwe, "hi"),
              ( 2, Just low, Just lowe, "lo"),
              ( 3, Nothing,  Nothing,   "a0"),
              ( 4, Nothing,  Nothing,   "a1"),
              ( 5, Nothing,  Nothing,   "a2"),
              ( 6, Nothing,  Nothing,   "a3"),
              ( 7, Just spw, Just spwe, "sp"),
              ( 8, Nothing,  Nothing,   "fp"),
              ( 9, Nothing,  Nothing,   "r0"),
              (10, Nothing,  Nothing,   "r1"),
              (11, Nothing,  Nothing,   "r2"),
              (12, Nothing,  Nothing,   "r3"),
              (13, Nothing,  Nothing,   "r4"),
              (14, Nothing,  Nothing,   "r5"),
              (15, Nothing,  Nothing,   "r6")]
       make_we (i, mwe) = do
           s1 <- select4_bit i writecmd
           s2 <- we &: s1
           case mwe of
               (Just we') -> we' |: s2
               Nothing    -> return s2
       make_w (i, Just w,  Just we') = we' <: (w, writewreg)
       make_w (i, Nothing, Nothing)  = return writewreg
       make_r (i,mw,mwe,nm) = do
           w  <- make_w (i,mw,mwe)
           we <- make_we (i,mwe)
           return $ make_register w we nm
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

-- Provides direct read-only access to the register which literal name is given
get_register :: String -> Var
get_register n = (n,16,Ereg $ n ++ "_temp")

