
module Test where
import NetList
import Data.Int
import Flags
import Utility

test_system fun r1 r2 = (afun,a1,a2,res)
 where (afun,a1,a2) = test_alu fun r1 r2
       res          = test_check_flags fun

-- TestCode Test    operation     FlagTest
--  0000     eq      r1 - r2        z
--  0001     g       r1 - r2      nc . nz
--  0010     ge      r1 - r2        nc
--  0011     l       r2 - r1      nc . nz
--  0100     le      r2 - r1        nc
--  0101     gi      r1 - r2      (no+p).(o+np).nz
--  0110     gei     r1 - r2      (no+p).(o+np)
--  0111     li      r2 - r1      (no+p).(o+np).nz
--  1000     lei     r2 - r1      (no+p).(o+np)
--  1001     andz    r1 . r2        z
--  1010     nandz   not(r1.r2)     z
--  1011     orz     r1 + r2        z
--  1100     norz    not(r1+r2)     z
--  1101     eq60    r1 - 60        z
--  1110     neq60   r1 - 60        nz
--

test_alu fun r1 r2 = runVM (make_gen "test_system_alu") $ do
    c60 <- constV 16 60

    l   <- select4_bit 3 fun
    le  <- select4_bit 4 fun
    li  <- select4_bit 7 fun
    lei <- select4_bit 8 fun

    b1'  <- l   |: le
    b1'' <- li  |: lei
    b1   <- b1' |: b1''
    a1   <- b1 <: (r2,r1)

    eq60  <- select4_bit 13 fun
    neq60 <- select4_bit 14 fun
    b2    <- eq60 |: neq60
    a2'   <- b2 <: (c60,r2)
    a2    <- b1 <: (r1,a2')

    alu_sub  <- constV 4  6
    alu_or   <- constV 4 10
    alu_and  <- constV 4  8
    alu_nand <- constV 4 11
    alu_xor  <- constV 4  9

    andz  <- select4_bit  9 fun
    nandz <- select4_bit 10 fun
    orz   <- select4_bit 11 fun
    xorz  <- select4_bit 12 fun

    cd1 <- andz  <: (alu_and,  alu_sub)
    cd2 <- nandz <: (alu_nand, cd1)
    cd3 <- orz   <: (alu_or,   cd2)
    cd  <- xorz  <: (alu_xor,  cd3)

    return (cd, a1, a2)

-- fun : x3x2\x1x0
--        00   01   11   10
--    00  z    ncz  ncz  nc
--    01  nc   sds  sds  sd
--    11  z    z    -    nz
--    10  sd   z    z    z

test_check_flags fun = runVM (make_gen "test_system_cflags") $ do
    let (z,p,c,o) = (get_flag "z", get_flag "p", get_flag "c", get_flag "o")
    nz    <- notv z
    np    <- notv p
    no    <- notv o
    nc    <- notv c
    ncz   <- nc &: nz
    no_p  <- no |: p
    o_np  <- o |: np
    sd    <- no_p &: o_np
    sds   <- sd &: nz
          
    x0    <- fun @: 0
    x1    <- fun @: 1
    x2    <- fun @: 2
    x3    <- fun @: 3
    nx0   <- notv x0
    nx1   <- notv x1
    nx2   <- notv x2
    nx3   <- notv x3
          
    tnz   <- select4_bit 14 fun
          
    tsd1  <- select4_bit 6 fun
    tsd2  <- select4_bit 8 fun
    tsd   <- tsd1 |: tsd2

    tnc1  <- select4_bit 2 fun
    tnc2  <- select4_bit 4 fun
    tnc   <- tnc1 |: tnc2

    tsds' <- select2_bit 1 x3 x2
    tsds  <- x1 &: tsds'

    tncz' <- select2_bit 0 x3 x2
    tncz  <- x1 &: tncz'

    r1    <- tnz  <: (nz,  z)
    r2    <- tsd  <: (sd,  r1)
    r3    <- tnc  <: (nc,  r2)
    r4    <- tsds <: (sds, r3)
    r5    <- tncz <: (ncz, r4)
    return r5

