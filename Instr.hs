
module Instructions where
import NetList
import Utility
import Registers

instruction_reader ins = do
    op   <- ins !!: (0,3)
    dest <- ins !!: (4,7)
    src  <- ins !!: (8,11)
    func <- ins !!: (12,15)
    return (op,dest,src,func)

pp_register w = constV 1 1 >>= \we -> return $ make_register w we "pp"
get_pp_register = return $ get_register "pp"

instruction_system_int test_func p1 p2 alu_res alu_hiw alu_hiwe alu_low alu_lowe
                       mem_reading mem_nap mem_esp mem_wsp
                       test_out =
 runVM (make_gen "instruction_system") $ do
    c1 <- constV 16 1
    c2 <- constV 16 2
    code_push <- constV 4 8

    pp  <- get_pp_register
    (pp1,_) <- full_adder 16 c1 pp
    (pp2,_) <- full_adder 16 c1 pp1
    ins <- rom 16 8 pp
    val <- rom 16 8 pp1
    (op,dest,src,func) <- instruction_reader ins
    is_long  <- op @: 3
    next_pp  <- is_long <: (pp2,pp1)
    next_ins <- rom 16 8 next_pp
    next_is_long <- next_ins @: 15

    -- Instruction decoding
    ibin <- select4_bit 0  op
    iun  <- select4_bit 1  op
    imem <- select4_bit 2  op
    ijfs <- select4_bit 4  op
    ijts <- select4_bit 5  op
    ijfl <- select4_bit 8  op
    ijtl <- select4_bit 9  op
    ijmp <- select4_bit 10 op
    ical <- select4_bit 11 op -- TODO
    immo <- select4_bit 12 op
    ilim <- select4_bit 13 op -- TODO

    -- ALU input
    is_test    <- ijts  |: ijtl
    is_alu     <- ibin  |: iun
    is_bin     <- is_test |: ibin
    real_func' <- is_test <: (test_func,func)
    real_func  <- ical <: (code_push,real_func')
    op1        <- is_test <: (p1,src)
    op2        <- is_test <: (p2,dest)
    whi        <- return alu_hiw
    ehi        <- alu_hiwe &: iun
    wlo        <- return alu_low
    elo        <- alu_lowe &: iun

    -- Memory system input
    mem_enable <- long_or [imem, immo, ical]
    esp        <- mem_esp &: mem_enable
    wsp        <- return mem_wsp
    wmem       <- return mem_nap
    addr'      <- mem_reading <: (src,dest)
    (addr'',_) <- full_adder 16 addr' val
    addr'''    <- is_long <: (addr'',addr')
    addr       <- ical <: (next_pp,addr''')

    -- Jump instructions
    is_jmp' <- is_test &: test_out
    is_jmp  <- long_or [is_jmp', ijmp, ijfs, ijfl]
    cad     <- next_is_long <: (c2,c1)
    (jpp,_) <- full_adder 16 cad next_pp
    jmpdest <- is_long <: (val,jpp)

    -- Registers operations
    rcmd <- return src
    wcmd <- return dest
    rgdt <- is_alu <: (alu_res,mem_nap)
    rgwe <- is_alu |: mem_reading

    -- Update pp
    pp_update <- is_jmp <: (jmpdest,next_pp)
    pp_register pp_update

    return (is_bin,real_func,op1,op2,whi,ehi,wlo,elo,
            wsp,esp,mem_enable,addr,
            rcmd,wcmd,rgdt,rgwe,undefined) -- TODO



