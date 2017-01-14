
module Instructions where
import NetList
import Utility
import Registers
import Memory
import Flags
import ALU
import Test
import Control.Monad.Fix

instruction_reader ins = do
    op   <- ins !!: (0,3)
    dest <- ins !!: (4,7)
    src  <- ins !!: (8,11)
    func <- ins !!: (12,15)
    return (op,dest,src,func)

pp_register w = constV 1 1 >>= \we -> return $ make_register w we "pp"
get_pp_register = return $ get_register "pp"

instruction_system_fix :: VarMonad (Var, Var, Var, Var, Var, Var, Var, Var, Var, Var,
                           Var, Var, Var, Var, Var, Var, (Var,Var,Var,Var,Var),
                           [Var], [Var])
instruction_system_fix = runVM (make_gen "instruction_system") $ mfix instruction_system'

--instruction_system test_func test_src test_dest test_out
--                   alu_res alu_hiw alu_hiwe alu_low alu_lowe
--                   mem_reading mem_nap mem_esp mem_wsp mem_ret
--                   flag_code_out =
-- runVM (make_gen "instruction_system") $ do
instruction_system' (is_bin_r, real_func_r, real_src_r, real_dest_r,
                     write_hi_r, enable_hi_r, write_lo_r, enable_lo_r,
                     wsp_r, esp_r, mem_enable_r, addr_r,
                     read_cmd_r, write_cmd_r, reg_data_r, reg_we_r,
                     flags_r, dps_r, flag_temp_r) = do
    (read_reg, write_reg, dps) <-
           register_manager read_cmd_r write_cmd_r reg_data_r reg_we_r
                            write_hi_r enable_hi_r write_lo_r enable_lo_r wsp_r esp_r
    (mem_reading, mem_nap, mem_esp, mem_wsp, mem_ret) <-
           memory_system real_func_r mem_enable_r real_src_r addr_r
    flag_code_out <-
           flag_code real_func_r
    (test_func, test_src, test_dest, test_out) <-
           test_system real_func_r real_src_r real_dest_r flags_r
    (alu_res, alu_wen, flags, flag_en) <-
           alu is_bin_r real_func_r real_src_r real_dest_r
    flag_temp <-
           flag_system flag_en flags_r
    alu_hiwe <- constV  1 0
    alu_lowe <- constV  1 0
    alu_hiw  <- constV 16 0
    alu_low  <- constV 16 0
    
    c1        <- constV 16 1
    c2        <- constV 16 2
    code_push <- constV 4 8

    pp                 <- get_pp_register
    (pp1,_)            <- full_adder 16 c1 pp
    (pp2,_)            <- full_adder 16 c1 pp1
    ins                <- rom 16 8 pp
    val                <- rom 16 8 pp1
    (op,dest,src,func) <- instruction_reader ins
    is_long            <- op @: 3
    next_pp            <- is_long <: (pp2,pp1)
    next_ins           <- rom 16 8 next_pp
    next_is_long       <- next_ins @: 3

    -- Instruction decoding
    ins_alu_bin         <- select4_bit  0 op -- binary ALU instruction
    ins_alu_un          <- select4_bit  1 op -- unary ALU instruction
    ins_mem             <- select4_bit  2 op -- memory instruction
    ins_jump_flag_short <- select4_bit  4 op -- jump on flag, short instruction
    ins_jump_test_short <- select4_bit  5 op -- jump on test, short instruction
    ins_jump_flag_long  <- select4_bit  8 op -- jump on flag, long instruction
    ins_jump_test_long  <- select4_bit  9 op -- jump on test, long intruction
    ins_jump            <- select4_bit 10 op -- jump instruction
    ins_call            <- select4_bit 11 op -- call instruction
    ins_mem_offset      <- select4_bit 12 op -- memory instruction with offset
    ins_limm            <- select4_bit 13 op -- load immediate instruction

    -- ALU input
    is_test    <- ins_jump_test_short  |: ins_jump_test_long
    is_alu     <- ins_alu_bin  |: ins_alu_un
    is_bin     <- is_test |: ins_alu_bin
    real_func' <- is_test <: (test_func,func)
    real_func  <- ins_call <: (code_push,real_func') -- In case of a call, let the memory system do the push
    real_src   <- is_test <: (test_src,src)
    real_dest  <- is_test <: (test_dest,dest)
    write_hi   <- return alu_hiw
    enable_hi  <- alu_hiwe &: ins_alu_un
    write_lo   <- return alu_low
    enable_lo  <- alu_lowe &: ins_alu_un

    -- Memory system input
    mem_enable <- long_or [ins_mem, ins_mem_offset, ins_call] -- In case of a call, let the memory system do the push
    esp        <- mem_enable &: mem_esp -- Enable sp if the memory system is active and if it says so
    wsp        <- return mem_wsp
    addr'      <- mem_reading <: (src,dest) -- If we're reading (ie not writing), the address is in src
                                            -- otherwise it's in dest
    (addr'',_) <- full_adder 16 addr' val -- Add the offset
    addr'''    <- is_long <: (addr'',addr') -- Consider the offset only if the instruction is long
    addr       <- ins_call <: (next_pp,addr''') -- In case of the call, what we write is the address of the
                                                -- next instruction

    -- Jump instructions
    is_jmp_test <- is_test &: test_out
    is_flag     <- ins_jump_flag_short |: ins_jump_flag_long
    is_jmp_flag <- is_flag &: flag_code_out
    is_jmp      <- long_or [is_jmp_test, is_jmp_flag, ins_jump]
    cad         <- next_is_long <: (c2,c1)
    (jpp,_)     <- full_adder 16 cad next_pp
    jump_on_ret <- mem_enable &: mem_ret
    jmpdest'    <- jump_on_ret <: (wsp,jpp)
    jump_on_val <- is_long |: ins_call
    jmpdest     <- jump_on_val <: (val,jmpdest')

    -- Registers operations
    read_cmd  <- return src
    write_cmd <- return dest
    reg_data' <- is_alu <: (alu_res,mem_nap)
    reg_data  <- ins_limm <: (val,reg_data')
    reg_we    <- is_alu |: mem_reading

    -- Update pp
    pp_update <- is_jmp <: (jmpdest,next_pp)
    pp_register pp_update

    return (is_bin, real_func, real_src, real_dest, write_hi, enable_hi, write_lo, enable_lo,
            wsp, esp, mem_enable, addr,
            read_cmd, write_cmd, reg_data, reg_we,
            flags, dps, flag_temp)



