
module Test where
import NetList
import Data.Int
import Flags
import Utility

test_system fun src dest flags = (afun,a1,a2,res)
 where (afun,a1,a2) = test_alu fun src dest
       res          = test_check_flags fun flags

-- TestCode   Test    operation          FlagTest
--    00       eq      xor(src, dest)      z
--    01       neq     dest - src          nz
--    02       g       src - dest         c . nz
--    03       ge      src - dest          c
--    04       l       dest - src         c . nz
--    05       le      dest - src          c
--    06       gi      dest - src        nz.(o == p)
--    07       gei     dest - src         o == p
--    08       li      src - dest        nz.(o == p)
--    09       lei     src - dest         o == p
--    10       andz    src . dest          z
--    11       nandz   nand(src,dest)      z
--    12       xorz    xor(src, dest)      z
--    13       nxorz   xor(src, not(dest)) z
--    14       eq60    xor(src, 60)        z
--    15       neq60   src - 60            nz

test_alu fun src dest = runVM (make_gen "test_system_alu") $ do
    alu_diff <- constV  4 10
    alu_xor  <- constV  4 14
    alu_nand <- constV  4 15
    alu_and  <- constV  4 12
    c60      <- constV 16 60
    ndest    <- notv dest

    cd <- long_select4 fun [
          ( 0, alu_xor)
        , ( 1, alu_diff)
        , ( 2, alu_diff)
        , ( 3, alu_diff)
        , ( 4, alu_diff)
        , ( 5, alu_diff)
        , ( 6, alu_diff)
        , ( 7, alu_diff)
        , ( 8, alu_diff)
        , ( 9, alu_diff)
        , (10, alu_and)
        , (11, alu_nand)
        , (12, alu_xor)
        , (13, alu_xor)
        , (14, alu_xor)
        , (15, alu_diff)
        ]

    op1 <- long_select4 fun [
          ( 0, src)
        , ( 1, src)
        , ( 2, dest)
        , ( 3, dest)
        , ( 4, src)
        , ( 5, src)
        , ( 6, src)
        , ( 7, src)
        , ( 8, dest)
        , ( 9, dest)
        , (10, src)
        , (11, src)
        , (12, src)
        , (13, src)
        , (14, c60)
        , (15, c60)
        ]


    op2 <- long_select4 fun [
          ( 0, dest)
        , ( 1, dest)
        , ( 2, src)
        , ( 3, src)
        , ( 4, dest)
        , ( 5, dest)
        , ( 6, dest)
        , ( 7, dest)
        , ( 8, src)
        , ( 9, src)
        , (10, dest)
        , (11, dest)
        , (12, dest)
        , (13, ndest)
        , (14, src)
        , (15, src)
        ]

    return (cd, op1, op2)

test_check_flags fun (z,c,p,o,s) = runVM (make_gen "test_system_cflags") $ do
    nz        <- notv z
    cz        <- c &: nz
    o_ne_p    <- o ^: p
    o_eq_p    <- notv o_ne_p
    nz_o_eq_p <- nz &: o_eq_p

    long_select4 fun [
          ( 0, z)
        , ( 1, nz)
        , ( 2, cz)
        , ( 3, c)
        , ( 4, cz)
        , ( 5, c)
        , ( 6, nz_o_eq_p)
        , ( 7, o_eq_p)
        , ( 8, nz_o_eq_p)
        , ( 9, o_eq_p)
        , (10, z)
        , (11, z)
        , (12, z)
        , (13, z)
        , (14, z)
        , (15, nz)
        ]

