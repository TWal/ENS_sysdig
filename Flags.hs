
module Flags where
import NetList

-------------------------------------------------------------------------------
----------------------------- Flag code ---------------------------------------
-------------------------------------------------------------------------------
flag_code (z,c,p,o,s) func = runVM (make_gen "flag_code") $ do
    must_neg <- func @: 0
    x1  <- func @: 1
    x2  <- func @: 2
    x3  <- func @: 3

    t1  <- x1 <: (c,z)
    t2  <- x1 <: (o,p)
    t3  <- x2 <: (t2,t1)
    t4  <- x3 <: (s,t3)
    t4' <- notv t4
    must_neg <: (t4',t4)

-------------------------------------------------------------------------------
---------------------------- Flag system --------------------------------------
-------------------------------------------------------------------------------
flag_system en (z,c,p,o,s) = trans
    (make_flag "flag_z" z en
    ,make_flag "flag_c" c en
    ,make_flag "flag_p" p en
    ,make_flag "flag_o" o en
    ,make_flag "flag_s" s en)
 where trans ((x1,y1), (x2,y2), (x3,y3), (x4,y4), (x5,y5)) =
             (x1, x2, x3, x4, x5, [y1, y2, y3, y4, y5])

make_flag nm w we = (rr,rt)
 where rt = (nm ++ "_temp", 1, Emux we w rr)
       rr = (nm, 1, Ereg $ nm ++ "_temp")

