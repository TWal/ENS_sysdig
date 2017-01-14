
module Main where
import NetList
import Data.Either
import Data.Int
import Control.Monad

import Utility
import Registers
import Memory
import Flags
import ALU
import Instructions
import Test
-- On Mux assumes 1 -> first choice

computer :: Netlist
computer = (dps ++ flag_temp, [], [])
 where ( read_reg, write_reg, dps) =
           register_manager rcmd wcmd reg_write_data reg_write_enable
                            whi wehi wlo welo wsp wesp
       ( mem_reading, mem_data, mem_wesp, mem_wsp, mem_ret) =
           memory_system fun mem_enable src addr
       flag_cd =
           flag_code fun
       flag_temp =
           flag_system flag_en flags
       ( test_fun, test_src, test_dst, test_result) =
           test_system test_fun src dest
       ( alu_res, alu_wen, flags, flag_en) =
           alu alu_bin fun src dest
       ( alu_bin, fun, src, dest
        , whi, wehi, wlo, welo, wsp, wesp
        , mem_enable, addr
        , rcmd, wcmd, reg_write_data, reg_write_enable) =
           instruction_system test_fun test_src test_dst test_result
                              alu_res alu_whi alu_wehi alu_wlo alu_welo
                              mem_reading mem_data mem_wesp mem_wsp mem_ret
                              flag_cd
       alu_wehi = vconstV "alu_wehi" 1 0
       alu_welo = vconstV "alu_welo" 1 0
       alu_whi  = vconstV "alu_whi" 16 0
       alu_wlo  = vconstV "alu_wlo" 16 0

netlist'' :: Netlist
netlist'' = (dps, [rd,rdt,get_register "sp"], [])
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
       (rd,rdt,spwe,spw,_) = memory_system we fun dt addr
       fun  = ("fun", 4, Einput)
       dt   = ("data", 16, Einput)
       addr = ("addr", 16, Einput)

aluNetlist :: Netlist
aluNetlist = (flagstmp,[renameV "out" out,renameV "wen" wen,renameV "z" z,renameV "c" c,
                        renameV "p" p,renameV "o" o,renameV "s" s,renameV "fen" fen],
               [])
  where (out,wen,(z,c,p,o,s),fen) = alu bin func op1 op2
        bin = inputV "bin" 1
        func = inputV "func" 4
        op1 = inputV "op1" 16
        op2 = inputV "op2" 16
        flagstmp = flag_system fen (z,c,p,o,s)

-------------------------------------------------------------------------------
----------------------------- Tests -------------------------------------------
-------------------------------------------------------------------------------
select4_bit_test :: Netlist
select4_bit_test = ([],[out1,out2],[])
 where inpt = inputV "input" 4
       out1 = runVM (make_gen "out1") $ select4_bit 3 inpt
       out2 = runVM (make_gen "out2") $ select4_bit 7 inpt

full_adder_test ::Netlist
full_adder_test = ([],[res,r],[])
 where a1      = inputV "a1" 64
       a2      = inputV "a2" 64
       (res,r) = runVM (make_gen "full_adder") $ full_adder 64 a1 a2

flag_system_test = (flags, [z,c,p,o,s], [])
 where inz         = inputV "inz"  1
       inc         = inputV "inc"  1
       inp         = inputV "inp"  1
       ino         = inputV "ino"  1
       ins         = inputV "ins"  1
       en          = inputV "en" 1
       flags       = flag_system en (inz, inc, inp, ino, ins)
       (z,c,p,o,s) = (get_flag "z", get_flag "c", get_flag "p", get_flag "o", get_flag "s")

flag_code_test = (flags, [out], [])
 where en    = inputV "en" 1
       win   = inputV "win" 5
       code  = inputV "code" 4
       wz    = ("wz", 1, Eselect 0 win)
       wc    = ("wc", 1, Eselect 1 win)
       wp    = ("wp", 1, Eselect 2 win)
       wo    = ("wo", 1, Eselect 3 win)
       ws    = ("ws", 1, Eselect 4 win)
       out   = flag_code code
       flags = flag_system en (wz,wc,wp,wo,ws)

register_manager_test = (regs, [rreg,rwreg], [])
 where rcmd  = inputV "rcmd"   4
       wcmd  = inputV "wcmd"   4
       write = inputV "write" 16
       we    = inputV "we"     1
       hiw   = inputV "hiw"   16
       hiwe  = inputV "hiwe"   1
       low   = inputV "low"   16
       lowe  = inputV "lowe"   1
       spw   = inputV "spw"   16
       spwe  = inputV "spwe"   1
       (rreg,rwreg,regs) = register_manager rcmd wcmd write we
                                            hiw hiwe low lowe spw spwe

main :: IO ()
main = putNetlist register_manager_test
