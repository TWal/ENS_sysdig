
module Memory where
import NetList
import Utility
import Registers

-- The paramters are the memory function (4-bit nap), an enable bit, the data
-- the write (if any) and the address to write to/read from.
-- Returns :
--   - reading : indicates if their has been a read operation (and thus if the
--               result must be stored)
--   - read_nap : the 16-bit value read
--   - (esp,wsp) : if and what to write in the sp register, in case their has
--                 been a stack operation.
--   - ret : indicates if the instruction was a ret.
memory_system fun en dt addr = runVM (make_gen "memory_system") $ do
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
    c0_1  <- constV 1 0
    cmp2  <- constV 16 65534 -- 2 complement
    is_r  <- select2_bit 0 fun3 fun2 -- is the instruction a reading
    is_w  <- select2_bit 1 fun3 fun2 -- is the instruction a writing
    is_c  <- select2_bit 2 fun3 fun2 -- is the instruction a stack operation

    -- Handle sp
    ret  <- select4_bit 10 fun
    pop  <- select4_bit  9 fun
    push <- select4_bit  8 fun
    ince <- pop |: ret
    esp  <- en &: is_c

    ad      <- ince <: (c2,cmp2)
    (_,wsp) <- adder sp ad

    -- Are we performing a read operation ?
    reading <- is_r |: pop

    -- Should we write the lower byte ?
    wl'   <- is_w |: push
    wl    <- en &: wl'
    -- Should we write the upper byte ?
    wh'   <- select2_bit 0 fun1 fun0
    wh''  <- is_w &: wh'
    wh''' <- push |: wh''
    wh    <- en &: wh'''

    -- The used addresses for the two byte read
    used_addr1'    <- ince <: (wsp,sp)
    used_addr1     <- is_c <: (used_addr1',addr)
    (_,used_addr2) <- oneadder used_addr1

    used_dt <- return dt
    datal' <- used_dt !!: (0,7)
    datah  <- used_dt !!: (8,15)

    sel'  <- select2_bit 2 fun1 fun0
    sel   <- is_w &: sel'
    datal <- sel <: (datah,datal')

    rdl   <- ram 16 8 used_addr1 wl used_addr1 datal
    rdh'  <- ram 16 8 used_addr2 wh used_addr2 datah

    rdbu <- select4_bit 1 fun 
    rdbi <- select4_bit 2 fun
    rdb  <- rdbu |: rdbi

    seli'    <- rdl @: 7
    seli     <- seli' &: rdbi
    rdh''    <- seli <: (cf1_8,c0_8)
    rdh      <- rdb <: (rdh'',rdh')
    read_nap <- rdh -: rdl

    return (reading,read_nap,esp,wsp,ret)

