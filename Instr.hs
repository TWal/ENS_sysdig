
module Instructions where
import NetList
import Utility
import Registers

instruction_reader ins = do
    op   <- ins !!: (12,15)
    dest <- ins !!: (8,11)
    src  <- ins !!: (4,7)
    func <- ins !!: (0,3)
    return (op,dest,src,func)

pp_register w = constV 1 1 >>= \we -> return $ make_register w we "pp"
get_pp_register = return $ get_register "pp"

instruction_system_int afunc p1 p2 alu_res alu_hiw alu_hiwe alu_low alu_lowe
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
    isl <- op @: 3
    npp <- isl <: (pp2,pp1)
    nis <- rom 16 8 npp
    nisl <- nis @: 15

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
    istst  <- ijts  |: ijtl
    isalu  <- ibin  |: iun
    bin    <- istst |: ibin
    ffunc' <- istst <: (afunc,func)
    ffunc  <- ical <: (code_push,ffunc')
    op1    <- istst <: (p1,src)
    op2    <- istst <: (p2,dest)
    whi    <- return alu_hiw
    ehi    <- alu_hiwe &: iun
    wlo    <- return alu_low
    elo    <- alu_lowe &: iun

    -- Memory system input
    meme    <- long_or [imem, immo, ical]
    esp     <- mem_esp &: meme
    wsp     <- return mem_wsp
    wmem    <- return mem_nap
    addr'   <- mem_reading <: (src,dest)
    (addr'',_) <- full_adder 16 addr' val
    addr''' <- isl <: (addr'',addr')
    addr    <- ical <: (npp,addr''')

    -- Jump instructions
    isjmp' <- istst &: test_out
    isjmp  <- long_or [isjmp', ijmp, ijfs, ijfl]
    cad <- nisl <: (c2,c1)
    (jpp,_) <- full_adder 16 cad npp
    jmpdest <- isl <: (val,jpp)

    -- Registers operations
    rcmd <- return src
    wcmd <- return dest
    rgdt <- isalu <: (alu_res,mem_nap)
    rgwe <- isalu |: mem_reading

    -- Update pp
    pp_update <- isjmp <: (jmpdest,npp)
    pp_register pp_update

    return (bin,ffunc,op1,op2,whi,ehi,wlo,elo,
            wsp,esp,meme,addr,
            rcmd,wcmd,rgdt,rgwe,undefined) -- TODO



