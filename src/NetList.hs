
module NetList where
import Data.Int
import Data.List
import Data.Maybe
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad as M

-- A variable is a size and a computing tree
type Var = (Int8,TVar)
data TVar =
    Einput String
  | Econst Int
  | Earg Var
  | Ereg Var
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
type LVar = (String,Int8,TLVar)
data TLVar =
    LEinput
  | LEconst Int
  | LEarg LVar
  | LEreg String
  | LEnot LVar
  | LEor LVar LVar
  | LEand LVar LVar
  | LExor LVar LVar
  | LEnand LVar LVar
  | LEmux LVar LVar LVar
  | LErom Int8 Int8 LVar
  | LEram Int8 Int8 LVar LVar LVar LVar
  | LEconcat LVar LVar
  | LEslice Int8 Int8 LVar
  | LEselect Int8 LVar
  deriving (Show)

size :: Var -> Int8
size (s,_) = s
label :: LVar -> String
label (l,_,_) = l

test_var_eq :: Var -> Var -> Bool
test_var_eq (s1,tv1) (s2,tv2) = if s1 /= s2 then False else
 case (tv1,tv2) of
  (Einput s1,Einput s2)           -> s1 == s2
  (Econst i,Econst j)             -> i == j
  (Earg x1,Earg x2)               -> test x1 x2
  (Ereg x1,Ereg x2)               -> False
  (Enot x1,Enot x2)               -> test x1 x2
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
 where test = test_var_eq
data EqVar = EqVar Var deriving (Show)
fromv :: Var -> EqVar
fromv v = EqVar v
instance Eq EqVar where
    (EqVar x) == (EqVar y) = test_var_eq x y

remove_double :: [Var] -> [Var] -> ([String],[LVar])
remove_double out l =
    let (tree,mp,_) = foldl (\(l,mp,c) -> \v -> let (nv,nmp,nc) = remove_double' v mp c in (nv : l, nmp, nc)) ([],[],0) l
    in (map (fst3 . fromJust . ((flip lookup) mp) . fromv) out, tree)
  where fst3 (a,_,_) = a
        remove_double' :: Var -> [(EqVar,LVar)] -> Int -> (LVar,[(EqVar,LVar)],Int)
        remove_double' x@(s,tv) mp c = case lookup (fromv x) mp of
             Just t    -> (t,mp,c)
             Nothing   -> ((l,s,ntv), (fromv x,(l,s,ntv)) : nmp, nc)
         where (ntv,nmp,nc) = case tv of
                Einput _    -> (LEinput,mp,c)
                Earg v      -> let (v',mp',c') = remove_double' v mp (c+1) in (LEarg v', mp', c')
                Econst i    -> (LEconst i,mp,c+1)
                Ereg v      -> let (v',mp',c') = remove_double' v mp (c+1)
                               in (LEreg (label v'),mp',c')
                Enot v      -> let (v',mp',c') = remove_double' v mp (c+1) in (LEnot v', mp', c')
                Eand v1 v2  -> let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (LEand v1' v2', mp'',c'')
                Eor  v1 v2  -> let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (LEor v1' v2', mp'', c'')
                Exor v1 v2  -> let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (LExor v1' v2', mp'',c'')
                Enand v1 v2 -> let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (LEnand v1' v2', mp'',c'')
                Econcat v1 v2 ->
                               let (v1',mp',c')   = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'') = remove_double' v2 mp' c' in
                               (LEconcat v1' v2', mp'',c'')
                Emux v1 v2 v3 ->
                               let (v1',mp',c')     = remove_double' v1 mp (c+1) in
                               let (v2',mp'',c'')   = remove_double' v2 mp' c' in
                               let (v3',mp''',c''') = remove_double' v3 mp'' c'' in
                               (LEmux v1' v2' v3', mp''',c''')
                Erom i1 i2 v ->
                               let (v',mp',c') = remove_double' v mp (c+1)
                               in (LErom i1 i2 v', mp', c')
                Eram i1 i2 v1 v2 v3 v4 ->
                               let (v1',mp1,c1) = remove_double' v1 mp  (c+1) in
                               let (v2',mp2,c2) = remove_double' v2 mp1 c1 in
                               let (v3',mp3,c3) = remove_double' v3 mp2 c2 in
                               let (v4',mp4,c4) = remove_double' v4 mp3 c3 in
                               (LEram i1 i2 v1' v2' v3' v4', mp4, c4)
                Eslice i1 i2 v ->
                               let (v',mp',c') = remove_double' v mp (c+1)
                               in (LEslice i1 i2 v', mp',c')
                Eselect i v -> let (v',mp',c') = remove_double' v mp (c+1)
                               in (LEselect i v', mp', c')
               l = case tv of
                    Einput nm -> nm
                    _         -> "__" ++ show c

writeNetlist :: [Var] -> [Var] -> [String] -> String
writeNetlist cmps vs other_outputs =
    let filter = remove_double vs in
    let (outputs,usedv) = filter $ cmps ++ vs in
    let (_,inputs,vars,eqs) = foldl (flip rdfs) ([],[],[],[]) $ usedv in
       "INPUT "    ++ sepBy ", " id       inputs
    ++ "\nOUTPUT " ++ sepBy ", " id       (outputs ++ other_outputs)
    ++ "\nVAR "    ++ sepBy ", " show_var vars
    ++ "\nIN\n"    ++ sepBy "\n" id       eqs
 where rdfs v@(l,s,_) x@(sns,ai,av,ae) = if elem l sns then x
                                         else dfs v (l:sns,ai,(l,s):av,ae)
       dfs (l,s,LEinput) (sns,ai,av,ae) = (sns,l:ai,av,ae)
       dfs (l,s,LEarg v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = " ++ label v):ae)
       dfs (l,s,LEconst i) (sns,ai,av,ae) =
           (sns,ai,av,(l ++ " = " ++ show i):ae)
       dfs (l,s,LEreg lb) (sns,ai,av,ae) =
           (sns,ai,av,(l ++ " = REG " ++ lb):ae)
       dfs (l,s,LEnot v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = NOT " ++ label v):ae)
       dfs (l,s,LEor v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = OR " ++ label v1 ++ " " ++ label v2):ae)
       dfs (l,s,LEand v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = AND " ++ label v1 ++ " " ++ label v2):ae)
       dfs (l,s,LExor v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = XOR " ++ label v1 ++ " " ++ label v2):ae)
       dfs (l,s,LEnand v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = NAND " ++ label v1 ++ " " ++ label v2):ae)
       dfs (l,s,LEmux v1 v2 v3) (sns,ai,av,ae) = rdfs v1 $ rdfs v2 $ rdfs v3
           (sns,ai,av,(l ++ " = MUX " ++ label v1 ++ " " ++ label v2 ++ " " ++ label v3):ae)
       dfs (l,s,LErom i1 i2 v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,
               (l ++ " = ROM " ++ show i1 ++ " " ++ show i2 ++ " " ++ label v):ae)
       dfs (l,s,LEram i1 i2 v1 v2 v3 v4) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
                                                       $ rdfs v3 $ rdfs v4
           (sns,ai,av,(l ++ " = RAM " ++ show i1
                         ++ " "       ++ show i2
                         ++ " "       ++ label v1
                         ++ " "       ++ label v2
                         ++ " "       ++ label v3
                         ++ " "       ++ label v4):ae)
       dfs (l,s,LEconcat v1 v2) (sns,ai,av,ae) = rdfs v1 $ rdfs v2
           (sns,ai,av,(l ++ " = CONCAT " ++ label v2 ++ " " ++ label v1):ae)
       dfs (l,s,LEslice i1 i2 v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = SLICE " ++ show i1 ++ " " ++ show i2
                         ++ " " ++ label v):ae)
       dfs (l,s,LEselect i v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = SELECT " ++ show i ++ " " ++ label v):ae)
       sepBy c s (h1:h2:t) = s h1 ++ c ++ sepBy c s (h2:t)
       sepBy _ s (h:[])    = s h
       sepBy _ _ []        = ""
       show_var (l,s) = if s == 1 then l
                        else l ++ " : " ++ show s

input :: String -> Int8 -> Var
input l s = (s,Einput l)

reg :: Var -> Var
reg v = (size v, Ereg v)

notv :: Var -> Var
notv v = (size v, Enot v)

(&:) :: Var -> Var -> Var
v1 &: v2 = if n1 == n2 then (n1, Eand v1 v2)
           else error "Anding differently sized variables"
 where n1 = size v1
       n2 = size v2

(|:) :: Var -> Var -> Var
v1 |: v2 = if n1 == n2 then (n1, Eor v1 v2)
           else error "Oring differently sized variables"
 where n1 = size v1
       n2 = size v2

(^:) :: Var -> Var -> Var
v1 ^: v2 = if n1 == n2 then (n1, Exor v1 v2)
           else error "Xoring differently sized variables"
 where n1 = size v1
       n2 = size v2

(!:) :: Var -> Var -> Var
v1 !: v2 = if n1 == n2 then (n1, Enand v1 v2)
           else error "Nanding differently sized variables"
 where n1 = size v1
       n2 = size v2

(<:) :: Var -> (Var, Var) -> Var
v1 <: (v2,v3) = if n1 /= 1 then error "Muxing on nap"
              else if n2 /= n3 then error "Muxing two differently sized variables"
              else (n2, Emux v1 v2 v3)
 where n1 = size v1
       n2 = size v2
       n3 = size v3

-- Check sizes of variables
rom :: Int8 -> Int8 -> Var -> Var
rom i1 i2 v = (i2, Erom i1 i2 v)
ram :: Int8 -> Int8 -> Var -> Var -> Var -> Var -> Var
ram i1 i2 v1 v2 v3 v4 = (i2, Eram i1 i2 v1 v2 v3 v4)

(-:) :: Var -> Var -> Var
v1 -: v2 = (size v1 + size v2, Econcat v1 v2)

(!!:) :: Var -> (Int8,Int8) -> Var
v !!: (i1,i2) = if i1 > i2 then error "Invalid range for splice"
                else if i2 >= size v then error "Too big bound on splice"
                else (i2 - i1 + 1, Eslice i1 i2 v)

(@:) :: Var -> Int8 -> Var
v @: i = if i >= size v then error "Select indice too big"
         else (1, Eselect i v)

constV :: Int8 -> Int -> Var
constV s i = (s, Econst i)

type Netlist = ([Var],[Var],[String]) -- to be calculated variables,out varables names, out names for debug

putNetlist :: Netlist -> IO ()
putNetlist (a,b,c) = putStrLn $ writeNetlist a b c
