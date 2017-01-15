
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
computer = ([pp] ++ dps ++ flag_temp ++ to_compute, [pp, reg_write_data_c, wcmd_c] ++ dps, [])
 where read_reg_d         = dummy "read_reg"         16
       write_reg_d        = dummy "write_reg"        16
       mem_reading_d      = dummy "mem_reading"       1
       mem_data_d         = dummy "mem_data"         16
       mem_wesp_d         = dummy "mem_wesp"          1
       mem_wsp_d          = dummy "mem_wsp"          16
       mem_ret_d          = dummy "mem_ret"           1
       flag_cd_d          = dummy "flag_code"         1
       test_fun_d         = dummy "test_fun"          4
       test_src_d         = dummy "test_src"         16
       test_dst_d         = dummy "test_dst"         16
       test_result_d      = dummy "test_result"       1
       alu_res_d          = dummy "alu_res"          16
       alu_wen_d          = dummy "alu_wen"           1
       z_d                = dummy "z"                 1
       c_d                = dummy "c"                 1
       p_d                = dummy "p"                 1
       o_d                = dummy "o"                 1
       s_d                = dummy "s"                 1
       flag_en_d          = dummy "flag_en"           1
       alu_bin_d          = dummy "alu_bin"           1
       fun_d              = dummy "fun"               4
       src_d              = dummy "src"              16
       dest_d             = dummy "dest"             16
       whi_d              = dummy "whi"              16
       wehi_d             = dummy "wehi"              1
       wlo_d              = dummy "wlo"              16
       welo_d             = dummy "welo"              1
       wsp_d              = dummy "wsp"              16
       wesp_d             = dummy "wesp"              1
       mem_enable_d       = dummy "mem_enable"        1
       addr_d             = dummy "addr"             16
       rcmd_d             = dummy "rcmd"              4
       wcmd_d             = dummy "wcmd"              4
       reg_write_data_d   = dummy "reg_write_data"   16
       reg_write_enable_d = dummy "reg_write_enable"  1
       fun_for_test_d     = dummy "fun_for_test"      4

       ( read_reg, write_reg, dps) =
           register_manager rcmd_d wcmd_d reg_write_data_d reg_write_enable_d
                            whi_d wehi_d wlo_d welo_d wsp_d wesp_d

       ( mem_reading, mem_data, mem_wesp, mem_wsp, mem_ret) =
           memory_system fun_d mem_enable_d src_d addr_d

       flag_cd =
           flag_code fun_d
       flag_temp =
           flag_system flag_en_d (z_d,c_d,p_d,o_d,s_d)

       ( test_fun, test_src, test_dst, test_result) =
           test_system fun_for_test read_reg_d write_reg_d (z_d,c_d,p_d,o_d,s_d)

       ( alu_res, alu_wen, (z,c,p,o,s), flag_en) =
           alu alu_bin_d fun_d dest_d src_d rcmd_d

       ( alu_bin, fun, fun_for_test, src, dest
        , whi, wehi, wlo, welo, wsp, wesp
        , mem_enable, addr
        , rcmd, wcmd, reg_write_data, reg_write_enable, pp) =
           instruction_system test_fun_d test_src_d test_dst_d test_result_d
                              alu_res_d alu_wen_d alu_whi_d alu_wehi_d alu_wlo_d alu_welo_d
                              mem_reading_d mem_data_d mem_wesp_d mem_wsp_d mem_ret_d
                              read_reg_d write_reg_d
                              flag_cd_d

       alu_wehi_d = vconstV "alu_wehi" 1 0
       alu_welo_d = vconstV "alu_welo" 1 0
       alu_whi_d  = vconstV "alu_whi" 16 0
       alu_wlo_d  = vconstV "alu_wlo" 16 0

       read_reg_c         = renameV "read_reg"         read_reg
       write_reg_c        = renameV "write_reg"        write_reg
       mem_reading_c      = renameV "mem_reading"      mem_reading
       mem_data_c         = renameV "mem_data"         mem_data
       mem_wesp_c         = renameV "mem_wesp"         mem_wesp
       mem_wsp_c          = renameV "mem_wsp"          mem_wsp
       mem_ret_c          = renameV "mem_ret"          mem_ret
       flag_cd_c          = renameV "flag_code"        flag_cd
       test_fun_c         = renameV "test_fun"         test_fun
       test_src_c         = renameV "test_src"         test_src
       test_dst_c         = renameV "test_dst"         test_dst
       test_result_c      = renameV "test_result"      test_result
       alu_res_c          = renameV "alu_res"          alu_res
       alu_wen_c          = renameV "alu_wen"          alu_wen
       z_c                = renameV "z"                z
       c_c                = renameV "c"                c
       p_c                = renameV "p"                p
       o_c                = renameV "o"                o
       s_c                = renameV "s"                s
       flag_en_c          = renameV "flag_en"          flag_en
       alu_bin_c          = renameV "alu_bin"          alu_bin
       fun_c              = renameV "fun"              fun
       src_c              = renameV "src"              src
       dest_c             = renameV "dest"             dest
       whi_c              = renameV "whi"              whi
       wehi_c             = renameV "wehi"             wehi
       wlo_c              = renameV "wlo"              wlo
       welo_c             = renameV "welo"             welo
       wsp_c              = renameV "wsp"              wsp
       wesp_c             = renameV "wesp"             wesp
       mem_enable_c       = renameV "mem_enable"       mem_enable
       addr_c             = renameV "addr"             addr
       rcmd_c             = renameV "rcmd"             rcmd
       wcmd_c             = renameV "wcmd"             wcmd
       reg_write_data_c   = renameV "reg_write_data"   reg_write_data
       reg_write_enable_c = renameV "reg_write_enable" reg_write_enable
       fun_for_test_c     = renameV "fun_for_test"     fun_for_test
       to_compute         =
           [ read_reg_c, write_reg_c,
             mem_reading_c, mem_data_c, mem_wesp_c, mem_wsp_c, mem_ret_c, flag_cd_c,
             test_fun_c, test_src_c, test_dst_c, test_result_c, alu_res_c, alu_wen_c,
             z_c, c_c, p_c, o_c, s_c, flag_en_c, alu_bin_c, fun_c, src_c, dest_c,
             whi_c, wehi_c, wlo_c, welo_c, wsp_c, wesp_c, mem_enable_c,
             addr_c, rcmd_c, wcmd_c, reg_write_data_c, reg_write_enable_c,
             fun_for_test_c ]

aluNetlist :: Netlist
aluNetlist = (flagstmp,[renameV "out" out,renameV "wen" wen,renameV "z" z,renameV "c" c,
                        renameV "p" p,renameV "o" o,renameV "s" s,renameV "fen" fen],
               [])
  where (out,wen,(z,c,p,o,s),fen) = alu bin func dest src imm
        bin = inputV "bin" 1
        func = inputV "func" 4
        dest = inputV "dest" 16
        src = inputV "src" 16
        imm = inputV "imm" 4
        flagstmp = flag_system fen (z,c,p,o,s)

-------------------------------------------------------------------------------
----------------------------- Tests -------------------------------------------
-------------------------------------------------------------------------------
select4_bit_test :: Netlist
select4_bit_test = ([],[out1,out2],[])
 where inpt = inputV "input" 4
       out1 = runVM (make_gen "out1") $ select4_bit 3 inpt
       out2 = runVM (make_gen "out2") $ select4_bit 7 inpt

adder_test ::Netlist
adder_test = ([],[res,r],[])
 where a1      = inputV "a1" 64
       a2      = inputV "a2" 64
       (r,res) = runVM (make_gen "adder") $ adder a1 a2

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

memory_system_test = ([("sp_temp", 16, Econst 42)], [reading,nap,spwe,spw,ret], [])
 where fun  = inputV "fun"   4
       en   = inputV "en"    1
       dt   = inputV "dt"   16
       addr = inputV "addr" 16
       (reading,nap,spwe,spw,ret) = memory_system fun en dt addr

memory_system_flag_test = (regs, [nap], ["sp_temp"])
 where rcmd  = vconstV "rcmd"  4 0
       wcmd  = vconstV "wcmd"  4 0
       write = vconstV "writ" 16 0
       we    = vconstV "we"    1  0
       (_,_,regs) = register_manager rcmd wcmd write we
                                     write we write we spw spwe
       fun   = inputV "fun"   4
       en    = inputV "en"    1
       dt    = inputV "dt"   16
       addr  = inputV "addr" 16
       (_,nap,spwe,spw,_) = memory_system fun en dt addr

{-}mult_test :: Netlist
mult_test = ([],[res],[])
  where res = mulu a b
        a   = inputV "a" 16
        b   = inputV "b" 16-}

test_system_test = (realfn : dflags,[res,afund],[])
 where (afun, a1, a2, res) = test_system fun r1 r2 flags
       dflags = flag_system en flags
       fun    = inputV "func"  4
       r1     = inputV "r1"   16
       r2     = inputV "r2"   16
       bin    = vconstV "bin"  1 1
       afund  = dummy "afun" 4
       (_, _, flags, en) = alu bin afund a1 a2 imm
       imm    = vconstV "immediate" 4 0
       realfn = renameV "afun" afun

mult_test = ([],[out],[])
  where out = runVM (make_gen "mult") $ mul c a b
        a = inputV "a" 16
        b = inputV "b" 16
        c = inputV "code" 4

main :: IO ()
main = putNetlist mult_test
