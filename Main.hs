
module Main where
import NetList
import Data.Either
import Data.Bits (shiftL)
import Data.Int
import Debug.Trace
import Control.Monad
-- On Mux assumes 1 -> first choice

netlist'' = (dps, [rd,rdt,get_register "sp"])
 where rcmd = ("rcmd", 4, Econst 0)
       wcmd = ("wcmd", 4, Econst 0)
       wval = ("wval", 16, Econst 0)
       we   = ("we",   1,  Econst 0)
       low  = ("low",  16, Econst 0)
       lowe = ("lowe", 1,  Econst 0)
       hiw  = ("hiw",  16, Econst 0)
       hiwe = ("hiwe", 1,  Econst 0)
       (_,_,dps) = register_manager rcmd wcmd wval we
                                    low lowe hiw hiwe spw spwe
       (rd,rdt,spwe,spw) = memory_system fun dt addr
       fun  = ("fun", 4, Einput)
       dt   = ("data", 16, Einput)
       addr = ("addr", 16, Einput)

-- Shows how to do recursive definition
netlist = (dps, [rr,rw])
 where rcmd = ("rcmd", 4,  Einput)
       wcmd = ("wcmd", 4,  Einput)
       wval = ("wval", 16, Einput)
       we   = ("we",   1,  Einput)
       low  = ("low",  16, Econst 0)
       lowe = ("lowe", 1,  Econst 0)
       hiw  = ("hiw",  16, Econst 0)
       hiwe = ("hiwe", 1,  Econst 0)
       spw  = ("spw",  16, Econst 0)
       spwe = ("spwe", 1,  Econst 0)
       real_write = ("real_write", 16, Eor wval rw)
       (rr,rw,dps) = register_manager rcmd wcmd real_write we
                                      low lowe hiw hiwe spw spwe

netlist' = (dps, [out])
 where (z,c,p,o,s,dps) = flag_system en (wz,wc,wp,wo,ws)
       en              = ("en", 1, Einput)
       win             = ("win", 5, Einput)
       code            = ("code", 4, Einput)
       wz              = ("wz", 1, Eselect 0 win)
       wc              = ("wc", 1, Eselect 1 win)
       wp              = ("wp", 1, Eselect 2 win)
       wo              = ("wo", 1, Eselect 3 win)
       ws              = ("ws", 1, Eselect 4 win)
       out             = flag_code (z,c,p,o,s) code

main :: IO ()
main = putStrLn $ (\(a,b) -> writeNetlist a b) netlist''

-------------------------------------------------------------------------------
--------------------------- Memory system -------------------------------------
-------------------------------------------------------------------------------
memory_system fun dt addr = runVM (make_gen "memory_system") $ do
    let sp = get_register "sp"
    fun0  <- fun @: 0
    fun1  <- fun @: 1
    fun2  <- fun @: 2
    fun3  <- fun @: 3
    c0    <- constV 16 0
    c1    <- constV 16 1
    c2    <- constV 16 2
    cf1_8 <- constV 8 0xff
    c0_8  <- constV 8 0
    cmp2  <- constV 16 65534 -- 2 complement
    is_r  <- select2_bit 0 fun3 fun2
    is_w  <- select2_bit 1 fun3 fun2
    is_c  <- select2_bit 2 fun3 fun2

    -- Are we performing a write operation ?
    reading'  <- select2_bit 1 fun1 fun0
    reading'' <- is_c &: reading'
    reading   <- is_r |: reading''

    -- Should we write the lower byte ?
    wl   <- select2_bit 0 fun1 fun0
    -- Should we write the upper byte ?
    wc   <- is_c &: wl
    wh'  <- notv fun1
    wh'' <- is_w &: wh'
    wh   <- wc |: wh''

    -- The used addresses for the two byte read
    used_addr1     <- is_c <: (sp,addr)
    (used_addr2,_) <- full_adder 16 used_addr1 c1

    used_dt <- is_c <: (sp,dt)
    datal' <- used_dt !!: (0,7)
    datah  <- used_dt !!: (8,15)

    sel'  <- select2_bit 2 fun1 fun0
    sel   <- fun2 &: sel'
    datal <- sel <: (datah,datal')

    rdl   <- ram 16 8 used_addr1 wl used_addr1 datal
    rdh'  <- ram 16 8 used_addr2 wh used_addr2 datah

    rdbu <- select4_bit 1 fun 
    rdbi <- select4_bit 2 fun
    rdb  <- rdbu |: rdbi

    seli' <- rdl @: 7
    seli  <- seli' &: rdbi
    rdh'' <- seli <: (cf1_8,c0_8)
    rdh   <- rdb <: (rdh'',rdh')
    read_nap <- rdl -: rdh

    ret  <- select4_bit 10 fun
    pop  <- select4_bit  9 fun
    push <- select4_bit  8 fun
    ince <- pop |: ret
    esp  <- push |: ince

    (incsp,_) <- full_adder 16 sp c2
    (decsp,_) <- full_adder 16 sp cmp2
    wsp       <- ince <: (incsp,decsp)

    return (reading,read_nap,esp,wsp)

-------------------------------------------------------------------------------
----------------------------- Flag code ---------------------------------------
-------------------------------------------------------------------------------
flag_code (z,c,p,o,s) func = runVM (make_gen "flag_code") $ do
    must_neg <- func @: 0
    x1  <- func @: 1
    x2  <- func @: 2
    x3  <- func @: 3

    t1  <- x1 <: (c,z)
    t2  <- x1 <: (o,p)
    t3  <- x2 <: (t2,t1)
    t4  <- x3 <: (s,t3)
    t4' <- notv t4
    must_neg <: (t4',t4)

-------------------------------------------------------------------------------
---------------------------- Flag system --------------------------------------
-------------------------------------------------------------------------------
flag_system en (z,c,p,o,s) = trans
    (make_flag "flag_z" z en
    ,make_flag "flag_c" c en
    ,make_flag "flag_p" p en
    ,make_flag "flag_o" o en
    ,make_flag "flag_s" s en)
 where trans ((x1,y1), (x2,y2), (x3,y3), (x4,y4), (x5,y5)) =
             (x1, x2, x3, x4, x5, [y1, y2, y3, y4, y5])

make_flag nm w we = (rr,rt)
 where rt = (nm ++ "_temp", 1, Emux we w rr)
       rr = (nm, 1, Ereg $ nm ++ "_temp")

-------------------------------------------------------------------------------
------------------------------ Registers --------------------------------------
-------------------------------------------------------------------------------

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

-- The register manager. Returns readrreg, readwreg, and the direct access
-- registers.
-- The direct access registers are in order of their numbers, ie Hi, Lo and sp
-- Thus the returned value is : (readrreg, readwreg, hi, lo, sp)
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

get_register :: String -> Var
get_register n = (n,16,Ereg $ n ++ "_temp")

-------------------------------------------------------------------------------
---------------------------------- Utilities ----------------------------------
-------------------------------------------------------------------------------

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

