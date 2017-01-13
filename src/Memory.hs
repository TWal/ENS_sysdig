
module Memory where
import NetList
import Utility
import Registers

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
    cmp2  <- constV 16 65534 -- 2 complement
    is_r  <- select2_bit 0 fun3 fun2
    is_w  <- select2_bit 1 fun3 fun2
    is_c  <- select2_bit 2 fun3 fun2

    -- Are we performing a read operation ?
    reading'  <- select2_bit 1 fun1 fun0
    reading'' <- is_c &: reading'
    reading   <- is_r |: reading''

    -- Should we write the lower byte ?
    wl'   <- select2_bit 0 fun1 fun0
    wl    <- en &: wl'
    -- Should we write the upper byte ?
    wc    <- is_c &: wl
    wh'   <- notv fun1
    wh''  <- is_w &: wh'
    wh''' <- wc |: wh''
    wh    <- en &: wh'''

    -- The used addresses for the two byte read
    used_addr1     <- is_c <: (sp,addr)
    (used_addr2,_) <- full_adder 16 used_addr1 c1

    used_dt <- is_c <: (sp,dt)
    datal' <- used_dt !!: (0,7)
    datah  <- used_dt !!: (8,15)

    sel'  <- select2_bit 2 fun1 fun0
    sel   <- fun2 &: sel'
    datal <- sel <: (datah,datal')

    rdl   <- ram 16 8 used_addr1 wl used_addr1 datal
    rdh'  <- ram 16 8 used_addr2 wh used_addr2 datah

    rdbu <- select4_bit 1 fun 
    rdbi <- select4_bit 2 fun
    rdb  <- rdbu |: rdbi

    seli' <- rdl @: 7
    seli  <- seli' &: rdbi
    rdh'' <- seli <: (cf1_8,c0_8)
    rdh   <- rdb <: (rdh'',rdh')
    read_nap <- rdl -: rdh

    ret  <- select4_bit 10 fun
    pop  <- select4_bit  9 fun
    push <- select4_bit  8 fun
    ince <- pop |: ret
    esp  <- push |: ince

    ad      <- ince <: (c2,cmp2)
    (wsp,_) <- full_adder 16 sp ad

    return (reading,read_nap,esp,wsp,ret)

