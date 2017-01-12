
module Flags where
import NetList

-------------------------------------------------------------------------------
----------------------------- Flag code ---------------------------------------
-------------------------------------------------------------------------------
flag_code func = runVM (make_gen "flag_code") $ do
    let (z,c,p,o,s) = (get_flag "z", get_flag "c", get_flag "p",
                       get_flag "o", get_flag "s")
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
flag_system en (z,c,p,o,s) = map (\(n,w) -> make_flag n w en)
    [ ("flag_z", z)
    , ("flag_c", c)
    , ("flag_p", p)
    , ("flag_o", o)
    , ("flag_s", s) ]

make_flag nm w we = rt
 where rt = (nm ++ "_temp", 1, Emux we w rr)
       rr = (nm, 1, Ereg $ nm ++ "_temp")

get_flag :: String -> Var
get_flag s = ("flag_" ++ s, 1, Ereg $ "flag_" ++ s ++ "_temp")

