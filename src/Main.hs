
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

computer = (dps ++ flag_temp, [])
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
       (rd,rdt,spwe,spw,_) = memory_system we fun dt addr
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
 where dps  = flag_system en (wz,wc,wp,wo,ws)
       en   = ("en", 1, Einput)
       win  = ("win", 5, Einput)
       code = ("code", 4, Einput)
       wz   = ("wz", 1, Eselect 0 win) 
       wc   = ("wc", 1, Eselect 1 win)
       wp   = ("wp", 1, Eselect 2 win)
       wo   = ("wo", 1, Eselect 3 win)
       ws   = ("ws", 1, Eselect 4 win)
       out  = flag_code code

aluNetlist :: ([Var],[Var])
aluNetlist = (flagstmp,[out,wen,z,c,p,o,s,fen])
  where (out,wen,(z,c,p,o,s),fen) = alu bin func op1 op2
        bin = inputV "bin" 1
        func = inputV "func" 4
        op1 = inputV "op1" 16
        op2 = inputV "op2" 16
        flagstmp = flag_system fen (z,c,p,o,s)

main :: IO ()
main = putStrLn $ (\(a,b) -> writeNetlist a b) aluNetlist

