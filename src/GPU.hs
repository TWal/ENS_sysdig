
module GPU where
import NetList
import Utility

-- The arguments are :
--   - val : the val field of the instruction
--   - src_data : the value of the register designated by the src field of the
--                instruction.
--   - enable
-- It returns a value that must be computed.
-- Two gpu instruction must not come one after the other
gpu_system val src_data enable = runVM (make_gen "gpu_system") $ do
    c1_2   <- constV 2 1
    c2_2   <- constV 2 2
    c0     <- constV 16 0
    false  <- constV 1 0
    addr32 <- constV 16 1

    instr <- val !!: (2,7)
    byte1 <- instr -: c1_2
    byte2 <- val !!: (8,15)
    datl  <- byte2 -: byte1
    dat   <- src_data -: datl

    code  <- ram 16 32 addr32 enable addr32 dat
    cdl   <- code @: 0
    cdh   <- code @: 1
    is_cd <- select2_bit 1 cdh cdl

    -- If the previous instruction was a GPU write and this one is not,
    -- validate the write by putting 2 in the first two bits, without
    -- changing the rest
    validate' <- notv enable
    validate  <- is_cd &: validate'
    addr8     <- constV 16 4
    ninstr    <- code !!: (2,7)
    ndat      <- ninstr -: c2_2
    ram 16 8 addr8 validate addr8 ndat


