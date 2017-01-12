
module Main where
import NetList
import Data.Either
import Data.Int
import Control.Monad

import Utility
import Registers
import Memory
import Flags
-- On Mux assumes 1 -> first choice

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
       (rd,rdt,spwe,spw,_) = memory_system fun dt addr
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

main :: IO ()
main = putStrLn $ (\(a,b) -> writeNetlist a b) netlist''

