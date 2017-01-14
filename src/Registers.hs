
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
make_register w we = (rr,rt)
 where rt = (16,Emux we w rr)
       rr = (16,Ereg rt)

-- The register manager. Returns readrreg, readwreg, and the direct access
-- registers.
-- The returned value is : (readrreg, readwreg, list of variables to compute)
-- TODO : ^ outdated commentary, update it
-- When contradictory write orders are given on direct access registers, the
-- direct access one is selected.
register_manager readcmd writecmd writewreg we
                 hiw hiwe low lowe spw spwe
 = let rs = map make_r reg in let rs' = map fst rs in
   let readrreg = dicho_select readcmd  rs' 0 3 in
   let readwreg = dicho_select writecmd rs' 0 3 in
   (readrreg, readwreg, rs', map snd rs)
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
       make_we (i, mwe) =
           let s1 = select4_bit i writecmd in
           let s2 = we &: s1 in
           case mwe of
               (Just we') -> we' |: s2
               Nothing    -> s2
       make_w (i, Just w,  Just we') = we' <: (w, writewreg)
       make_w (i, Nothing, Nothing)  = writewreg
       make_r (i,mw,mwe,nm) =
           let w  = make_w (i,mw,mwe) in
           let we = make_we (i,mwe) in
           make_register w we
       mshift :: Int -> Int8 -> Int
       mshift x i = shiftL x $ fromIntegral i
       dicho_select :: Var -> [Var] -> Int -> Int8 -> Var
       dicho_select s l x i =
           if i == 0 then
               let sl = s @: 0 in
               sl <: (l!!(x+1),l!!x)
           else
               let n1 = dicho_select s l x $ i - 1 in
               let n2 = dicho_select s l (x + 1 `mshift` i) $ i - 1 in
               let sl = s @: i in
               sl <: (n2,n1)

