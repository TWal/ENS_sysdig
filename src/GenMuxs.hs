module GenMuxs (genMuxs) where
import NetList
import Language.Haskell.TH
import Control.Monad


-- This is not in `Utility.hs` because ghc requires to
-- instanciate templates in an other file

muxN :: Int -> Q Exp
muxN n = do
    v1s  <- replicateM n (newName "v1")
    v2s  <- replicateM n (newName "v2")
    ress <- replicateM n (newName "res")
    cond <- newName "cond"
    let tup1 = TupP (map VarP v1s)
    let tup2 = TupP (map VarP v2s)
    let args = [VarP cond, TupP [tup1, tup2]]
    let commands = zipWith3 (\v1 v2 res -> BindS (VarP res) (InfixE (Just $ VarE cond) (VarE $ mkName "<:") (Just $ TupE [VarE v1, VarE v2]))) v1s v2s ress
    let lastCommand = NoBindS (AppE (VarE (mkName "return")) (TupE (map VarE ress)))
    return $ LamE args (DoE (commands ++ [lastCommand]))

genMuxs :: Int -> Q [Dec]
genMuxs n = forM [2..n] mkMuxDec
  where
    mkMuxDec i = do
        func <- muxN i
        let name = mkName $ "<" ++ replicate i ':' ++ ""
        return $ FunD name [Clause [] (NormalB func) []]

