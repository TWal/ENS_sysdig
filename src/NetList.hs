
module NetList where
import Data.Int
import Data.List
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Monad as M

-- A variable is a size and a computing tree
-- The string uniquely identify the var
type Var = (String,Int8,TVar)
data TVar =
    Einput
  | Econst Int
  | Earg Var
  | Ereg String
  | Enot Var
  | Eor Var Var
  | Eand Var Var
  | Exor Var Var
  | Enand Var Var
  | Emux Var Var Var
  | Erom Int8 Int8 Var
  | Eram Int8 Int8 Var Var Var Var
  | Econcat Var Var
  | Eslice Int8 Int8 Var
  | Eselect Int8 Var
  deriving (Show)



make_gen :: String -> [String]
make_gen p = map (\c -> "__" ++ p ++ "_" ++ show c) [1..]

type VarMonad = S.StateT ([(EqVar,Var)],[String]) (Either String)
nlabel :: VarMonad String
nlabel = do
    (m,l) <- S.get
    S.put (m,tail l)
    return $ head l

size :: Var -> Int8
size (_,s,_) = s
label :: Var -> String
label (l,_,_) = l

test_var_eq :: (Int8,TVar) -> (Int8,TVar) -> Bool
test_var_eq (s1,tv1) (s2,tv2) = if s1 /= s2 then False else
 case (tv1,tv2) of
  (Econst i,Econst j) -> i == j
  (Earg x1,Earg x2)   -> test x1 x2
  (Ereg x1,Ereg x2)   -> x1 == x2
  (Enot x1,Enot x2)   -> test x1 x2
  (Eor x1 x2, Eor y1 y2)          -> test x1 y1 && test x2 y2
  (Eand x1 x2, Eand y1 y2)        -> test x1 y1 && test x2 y2
  (Exor x1 x2, Exor y1 y2)        -> test x1 y1 && test x2 y2
  (Enand x1 x2, Enand y1 y2)      -> test x1 y1 && test x2 y2
  (Emux x1 x2 x3,Emux y1 y2 y3)   -> test x1 y1 && test x2 y2 && test x3 y3
  (Erom i1 i2 x,Erom j1 j2 y)     -> i1 == j1 && i2 == j2 && test x y
  (Eram i1 i2 x1 x2 x3 x4, Eram j1 j2 y1 y2 y3 y4)
                                  -> i1 == j1 && i2 == j2 && test x1 y1
                                  && test x2 y2 && test x3 y3 && test x4 y4
  (Econcat x1 x2, Econcat y1 y2)  -> test x1 y1 && test x2 y2
  (Eslice i1 i2 x,Eslice j1 j2 y) -> i1 == j1 && i2 == j2 && test x y
  (Eselect i x, Eselect j y)      -> i == j && test x y
  _                               -> False
 where test (_,s1,t1) (_,s2,t2) = test_var_eq (s1,t1) (s2,t2)
data EqVar = EqVar (Int8,TVar) deriving (Show)
fromv :: Var -> EqVar
fromv (_,s,t) = EqVar (s,t)
instance Eq EqVar where
    (EqVar x) == (EqVar y) = test_var_eq x y

remove_double :: [String] -> [Var] -> [Var]
remove_double secure l = fst3 $ foldl (\(l,mp,c) -> \v -> let (nv,nmp,nc) = remove_double' v mp c in (nv : l, nmp, nc)) ([],[],0) l
  where fst3 (a,_,_) = a
        remove_double' :: Var -> [(EqVar,Var)] -> Int -> (Var,[(EqVar,Var)],Int)
        remove_double' x@(ol,s,tv) mp c = if l `elem` secure
            then ((ol,s,ntv),(fromv x,(ol,s,ntv)) : nmp,c)
            else case lookup (fromv x) mp of
             Just t    -> (t,mp,c)
             Nothing   -> ((l,s,ntv),(fromv x,(l,s,ntv)) : nmp,nc)
         where (ntv,nmp,nc) = case tv of
                Einput      -> (tv,mp,c+1)
                Earg v      -> let (v',mp',c') = remove_double' v mp (c+1) in (Earg v', mp', c')
                Econst i    -> (tv,mp,c+1)
                Ereg s      -> (tv,mp,c+1)
                Enot v      -> let (v',mp',c') = remove_double' v mp (c+1) in (Enot v', mp', c')
                Eand v1 v2  -> let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (Eand v1' v2', mp'',c'')
                Eor  v1 v2  -> let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (Eor v1' v2', mp'', c'')
                Exor v1 v2  -> let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (Exor v1' v2', mp'',c'')
                Enand v1 v2 -> let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (Enand v1' v2', mp'',c'')
                Econcat v1 v2 ->
                               let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (Econcat v1' v2', mp'',c'')
                Emux v1 v2 v3 ->
                               let (v1',mp',c')     = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'')   = remove_double' v2 mp' c' in
                               let (v3',mp''',c''') = remove_double' v3 mp'' c'' in
                               (Emux v1' v2' v3', mp''',c''')
                Erom i1 i2 v ->
                               let (v',mp',c') = remove_double' v mp (c+1)
                               in (Erom i1 i2 v', mp', c')
                Eram i1 i2 v1 v2 v3 v4 ->
                               let (v1',mp1,c1) = remove_double' v1 mp  (c+1) in
                               let (v2',mp2,c2) = remove_double' v2 mp1 c1 in
                               let (v3',mp3,c3) = remove_double' v3 mp2 c2 in
                               let (v4',mp4,c4) = remove_double' v4 mp3 c3 in
                               (Eram i1 i2 v1' v2' v3' v4', mp4, c4)
                Eslice i1 i2 v ->
                               let (v',mp',c') = remove_double' v mp (c+1)
                               in (Eslice i1 i2 v', mp',c')
                Eselect i v -> let (v',mp',c') = remove_double' v mp (c+1)
                               in (Eselect i v', mp', c')
               l = "__" ++ show c


writeNetlist :: [Var] -> [Var] -> [String] -> String
writeNetlist cmps vs other_outputs =
    let outputs = map label vs in
    let filter = remove_double outputs in
    -- let filter = id in
    let usedv = filter $ cmps ++ vs in
    let (_,inputs,vars,eqs) = foldl (flip rdfs) ([],[],[],[]) $ usedv in
       "INPUT "    ++ sepBy ", " id       inputs
    ++ "\nOUTPUT " ++ sepBy ", " id       (outputs ++ other_outputs)
    ++ "\nVAR "    ++ sepBy ", " show_var vars
    ++ "\nIN\n"    ++ sepBy "\n" id       eqs
 where rdfs v@(l,s,_) x@(sns,ai,av,ae) = if elem l sns then x
                                         else dfs v (l:sns,ai,(l,s):av,ae)
       dfs (l,s,Einput) (sns,ai,av,ae) = (sns,l:ai,av,ae)
       dfs (l,s,Earg v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = " ++ label v):ae)
       dfs (l,s,Econst i) (sns,ai,av,ae) =
           (sns,ai,av,(l ++ " = " ++ show i):ae)
       dfs (l,s,Ereg lb) (sns,ai,av,ae) =
           (sns,ai,av,(l ++ " = REG " ++ lb):ae)
       dfs (l,s,Enot v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = NOT " ++ label v):ae)
       dfs (l,s,Eor v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = OR " ++ label v1 ++ " " ++ label v2):ae)
       dfs (l,s,Eand v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = AND " ++ label v1 ++ " " ++ label v2):ae)
       dfs (l,s,Exor v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = XOR " ++ label v1 ++ " " ++ label v2):ae)
       dfs (l,s,Enand v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = NAND " ++ label v1 ++ " " ++ label v2):ae)
       dfs (l,s,Emux v1 v2 v3) (sns,ai,av,ae) = rdfs v1 $ rdfs v2 $ rdfs v3
           (sns,ai,av,(l ++ " = MUX " ++ label v1 ++ " " ++ label v2 ++ " " ++ label v3):ae)
       dfs (l,s,Erom i1 i2 v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,
               (l ++ " = ROM " ++ show i1 ++ " " ++ show i2 ++ " " ++ label v):ae)
       dfs (l,s,Eram i1 i2 v1 v2 v3 v4) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
                                                       $ rdfs v3 $ rdfs v4
           (sns,ai,av,(l ++ " = RAM " ++ show i1
                         ++ " "       ++ show i2
                         ++ " "       ++ label v1
                         ++ " "       ++ label v2
                         ++ " "       ++ label v3
                         ++ " "       ++ label v4):ae)
       dfs (l,s,Econcat v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = CONCAT " ++ label v2 ++ " " ++ label v1):ae)
       dfs (l,s,Eslice i1 i2 v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = SLICE " ++ show i1 ++ " " ++ show i2
                         ++ " " ++ label v):ae)
       dfs (l,s,Eselect i v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = SELECT " ++ show i ++ " " ++ label v):ae)
       sepBy c s (h1:h2:t) = s h1 ++ c ++ sepBy c s (h2:t)
       sepBy _ s (h:[])    = s h
       sepBy _ _ []        = ""
       show_var (l,s) = if s == 1 then l
                        else l ++ " : " ++ show s

runVM :: [String] -> VarMonad a -> VarMonad a
runVM s m = do
    (mn,l) <- S.get
    S.put (mn,s)
    ret <- m
    (mn',_) <- S.get
    S.put (mn',l)
    return ret

singleton :: Var -> VarMonad [Var]
singleton v = return [v]
outputs :: [VarMonad Var] -> VarMonad [Var]
outputs vs = M.sequence vs

create :: (Int8,TVar) -> VarMonad Var
create (s,t) = do
    (m,l) <- S.get
    case lookup (EqVar (s,t)) m of
        Just v  -> return v
        Nothing -> do
            let v = (head l, s, t)
            S.put ((EqVar (s,t), v) : m, tail l)
            return v

input :: String -> Int8 -> VarMonad Var
input l s = return (l,s,Einput)

copy :: Var -> VarMonad Var
copy (_,s,t) = create (s,t)

reg :: Var -> VarMonad Var
reg v = create (size v, Ereg $ label v)

notv :: Var -> VarMonad Var
notv v = create (size v, Enot v)

(&:) :: Var -> Var -> VarMonad Var
v1 &: v2 = if n1 == n2 then create (n1, Eand v1 v2)
           else fail "Anding differently sized variables"
 where n1 = size v1
       n2 = size v2

(|:) :: Var -> Var -> VarMonad Var
v1 |: v2 = if n1 == n2 then create (n1, Eor v1 v2)
           else fail "Oring differently sized variables"
 where n1 = size v1
       n2 = size v2

(^:) :: Var -> Var -> VarMonad Var
v1 ^: v2 = if n1 == n2 then create (n1, Exor v1 v2)
           else fail "Xoring differently sized variables"
 where n1 = size v1
       n2 = size v2

(!:) :: Var -> Var -> VarMonad Var
v1 !: v2 = if n1 == n2 then create (n1, Enand v1 v2)
           else fail "Nanding differently sized variables"
 where n1 = size v1
       n2 = size v2

(<:) :: Var -> (Var, Var) -> VarMonad Var
v1 <: (v2,v3) = if n1 /= 1 then fail "Muxing on nap"
              else if n2 /= n3 then fail "Muxing two differently sized variables"
              else create (n2, Emux v1 v2 v3)
 where n1 = size v1
       n2 = size v2
       n3 = size v3

-- Check sizes of variables
rom :: Int8 -> Int8 -> Var -> VarMonad Var
rom i1 i2 v = create (i2, Erom i1 i2 v)
ram :: Int8 -> Int8 -> Var -> Var -> Var -> Var -> VarMonad Var
ram i1 i2 v1 v2 v3 v4 = create (i2, Eram i1 i2 v1 v2 v3 v4)

(-:) :: Var -> Var -> VarMonad Var
v1 -: v2 = create (size v1 + size v2, Econcat v1 v2)

(!!:) :: Var -> (Int8,Int8) -> VarMonad Var
v !!: (i1,i2) = if i1 > i2 then fail "Invalid range for splice"
                else if i2 >= size v then fail "Too big bound on splice"
                else create (i2 - i1 + 1, Eslice i1 i2 v)

(@:) :: Var -> Int8 -> VarMonad Var
v @: i = if i >= size v then fail "Select indice too big"
         else create (1, Eselect i v)

constV :: Int8 -> Int -> VarMonad Var
constV s i = create (s, Econst i)

type Netlist = VarMonad ([Var],[Var],[String])

putNetlist :: Netlist -> IO ()
putNetlist md = case S.evalStateT md ([], make_gen "main") of
    Left e        -> fail $ "Error : " ++ e
    Right (a,b,c) -> putStrLn $ writeNetlist a b c
