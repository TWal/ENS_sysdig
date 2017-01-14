
module Main where
import NetList
import Data.Either
import Data.Int
import Control.Monad
import Control.Monad.Fix

import Utility
import Registers
import Memory
import Flags
import ALU
import Instructions
import Test
-- On Mux assumes 1 -> first choice

-- computer :: Netlist
-- computer = (dps ++ flag_temp, [], [])
--  where ( read_reg, write_reg, dps) =
--            register_manager rcmd wcmd reg_write_data reg_write_enable
--                             whi wehi wlo welo wsp wesp
--        ( mem_reading, mem_data, mem_wesp, mem_wsp, mem_ret) =
--            memory_system fun mem_enable src addr
--        flag_cd =
--            flag_code fun
--        flag_temp =
--            flag_system flag_en flags
--        ( test_fun, test_src, test_dst, test_result) =
--            test_system test_fun src dest flags
--        ( alu_res, alu_wen, flags, flag_en) =
--            alu alu_bin fun src dest
--        ( alu_bin, fun, src, dest
--         , whi, wehi, wlo, welo, wsp, wesp
--         , mem_enable, addr
--         , rcmd, wcmd, reg_write_data, reg_write_enable) =
--            instruction_system test_fun test_src test_dst test_result
--                               alu_res alu_whi alu_wehi alu_wlo alu_welo
--                               mem_reading mem_data mem_wesp mem_wsp mem_ret
--                               flag_cd
--        alu_wehi = constV 1 0
--        alu_welo = constV 1 0
--        alu_whi  = constV 16 0
--        alu_wlo  = constV 16 0

aluNetlist :: Netlist
aluNetlist = do
    bin  <- inputV "bin" 1
    func <- inputV "func" 4
    op1  <- inputV "op1" 16
    op2  <- inputV "op2" 16
    (out,wen,(z,c,p,o,s),fen) <- alu bin func op1 op2
    flagstmp <- flag_system fen (z,c,p,o,s)
    return $ (flagstmp,[renameV "out" out,renameV "wen" wen,renameV "z" z,renameV "c" c,
                        renameV "p" p,renameV "o" o,renameV "s" s,renameV "fen" fen],
               [])

-------------------------------------------------------------------------------
----------------------------- Tests -------------------------------------------
-------------------------------------------------------------------------------
select4_bit_test :: Netlist
select4_bit_test = do
    inpt <- inputV "input" 4
    out1 <- select4_bit 3 inpt
    out2 <- select4_bit 7 inpt
    return ([],[out1,out2],[])

full_adder_test ::Netlist
full_adder_test = do
    a1      <- inputV "a1" 64
    a2      <- inputV "a2" 64
    (res,r) <- runVM (make_gen "full_adder") $ full_adder 64 a1 a2
    return  ([],[res,r],[])

flag_system_test = do
    inz   <- inputV "inz"  1
    inc   <- inputV "inc"  1
    inp   <- inputV "inp"  1
    ino   <- inputV "ino"  1
    ins   <- inputV "ins"  1
    en    <- inputV "en"   1
    flags <- flag_system en (inz, inc, inp, ino, ins)
    let (z,c,p,o,s) = (get_flag "z", get_flag "c", get_flag "p", get_flag "o", get_flag "s")
    return (flags, [z,c,p,o,s], [])

flag_code_test = do
    en    <- inputV "en" 1
    win   <- inputV "win" 5
    code  <- inputV "code" 4
    wz    <- win @: 0
    wc    <- win @: 1
    wp    <- win @: 2
    wo    <- win @: 3
    ws    <- win @: 4
    out   <- flag_code code
    flags <- flag_system en (wz,wc,wp,wo,ws)
    return (flags, [out], [])

register_manager_test = do
    rcmd  <- inputV "rcmd"   4
    wcmd  <- inputV "wcmd"   4
    write <- inputV "write" 16
    we    <- inputV "we"     1
    hiw   <- inputV "hiw"   16
    hiwe  <- inputV "hiwe"   1
    low   <- inputV "low"   16
    lowe  <- inputV "lowe"   1
    spw   <- inputV "spw"   16
    spwe  <- inputV "spwe"   1
    (rreg,rwreg,regs) <- register_manager rcmd wcmd write we
                                          hiw hiwe low lowe spw spwe
    return (regs, [rreg,rwreg], [])

memory_system_test = do
    fun  <- inputV "fun"   4
    en   <- inputV "en"    1
    dt   <- inputV "dt"   16
    addr <- inputV "addr" 16
    (reading,nap,spwe,spw,ret) <- memory_system fun en dt addr
    return ([("sp_temp", 16, Econst 42)], [reading,nap,spwe,spw,ret], [])

memory_system_flag_test = do
    rcmd  <- constV  4 0
    wcmd  <- constV  4 0
    write <- constV 16 0
    we    <- constV    1  0
    fun   <- inputV "fun"   4
    en    <- inputV "en"    1
    dt    <- inputV "dt"   16
    addr  <- inputV "addr" 16
    (_,nap,spwe,spw,_) <- memory_system fun en dt addr
    (_,_,regs) <- register_manager rcmd wcmd write we
                                   write we write we spw spwe
    return (regs, [nap], ["sp_temp"])

test_system_test = (liftM fst) $ mfix $ \(_, flags_r) -> do
    fun <- inputV "func"  4
    r1  <- inputV "r1"   16
    r2  <- inputV "r2"   16
    bin <- constV  1 1
    (afun, a1, a2, res) <- test_system fun r1 r2 flags_r
    (_, _, flags, en)   <- alu bin afun a1 a2
    dflags              <- flag_system en flags
    return ((dflags, [res,afun,a1,a2], []), flags)

main :: IO ()
main = putNetlist test_system_test
