
module NetList where
import Data.Int
import Data.List
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad as M

-- A variable is a size and a computing tree
-- The string uniquely identify the var
type Var = (String,Int8,TVar)
data TVar =
    Einput
  | Edummy
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

type VarMonad = S.StateT [String] (Either String)
nlabel :: VarMonad String
nlabel = do
    c <- S.get
    S.put $ tail c
    return $ head c

size :: Var -> Int8
size (_,s,_) = s
label :: Var -> String
label (l,_,_) = l
is_dummy :: Var -> Bool
is_dummy (_,_,Edummy) = True
is_dummy _            = False

test_var_eq :: Int -> Var -> Var -> Bool
test_var_eq 0 _ _ = False
test_var_eq n (l1,s1,tv1) (l2,s2,tv2) = if l1 == l2 then True  else
                                        if s1 /= s2 then False else
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
 where test = test_var_eq $ n - 1
data EqVar = EqVar Var deriving (Show)
fromv :: Var -> EqVar
fromv v = EqVar v
instance Eq EqVar where
    (EqVar x) == (EqVar y) = test_var_eq 10000 x y

remove_double :: [String] -> [Var] -> [Var]
remove_double secure l = fst $ foldl (\(l,mp) -> \v -> let (nv,nmp) = remove_double' v mp in (nv : l, nmp)) ([],[]) l
  where remove_double' :: Var -> [(EqVar,Var)] -> (Var,[(EqVar,Var)])
        remove_double' x@(l,s,tv) mp = if l `elem` secure then ((l,s,ntv),(fromv x,(l,s,ntv)) : nmp)
            else case lookup (fromv x) mp of
             Just t    -> (t,mp)
             Nothing   -> ((l,s,ntv),(fromv x,(l,s,ntv)) : nmp)
         where (ntv,nmp) = case tv of
                Einput      -> (tv,mp)
                Edummy      -> (tv,mp)
                Earg v      -> let (v',mp') = remove_double' v mp in (Earg v', mp')
                Econst i    -> (tv,mp)
                Ereg s      -> (tv,mp)
                Enot v      -> let (v',mp') = remove_double' v mp in (Enot v', mp')
                Eand v1 v2  -> let (v1',mp')  = remove_double' v1 mp in
                               let (v2',mp'') = remove_double' v2 mp' in
                               (Eand v1' v2', mp'')
                Eor  v1 v2  -> let (v1',mp')  = remove_double' v1 mp in
                               let (v2',mp'') = remove_double' v2 mp' in
                               (Eor v1' v2', mp'')
                Exor v1 v2  -> let (v1',mp')  = remove_double' v1 mp in
                               let (v2',mp'') = remove_double' v2 mp' in
                               (Exor v1' v2', mp'')
                Enand v1 v2 -> let (v1',mp')  = remove_double' v1 mp in
                               let (v2',mp'') = remove_double' v2 mp' in
                               (Enand v1' v2', mp'')
                Econcat v1 v2 ->
                               let (v1',mp')  = remove_double' v1 mp in
                               let (v2',mp'') = remove_double' v2 mp' in
                               (Econcat v1' v2', mp'')
                Emux v1 v2 v3 ->
                               let (v1',mp')   = remove_double' v1 mp in
                               let (v2',mp'')  = remove_double' v2 mp' in
                               let (v3',mp''') = remove_double' v3 mp'' in
                               (Emux v1' v2' v3', mp''')
                Erom i1 i2 v ->
                               let (v',mp') = remove_double' v mp in (Erom i1 i2 v', mp')
                Eram i1 i2 v1 v2 v3 v4 ->
                               let (v1',mp1) = remove_double' v1 mp in
                               let (v2',mp2) = remove_double' v2 mp1 in
                               let (v3',mp3) = remove_double' v3 mp2 in
                               let (v4',mp4) = remove_double' v4 mp3 in
                               (Eram i1 i2 v1' v2' v3' v4', mp4)
                Eslice i1 i2 v ->
                               let (v',mp') = remove_double' v mp in (Eslice i1 i2 v', mp')
                Eselect i v -> let (v',mp') = remove_double' v mp in (Eselect i v', mp')


writeNetlist :: [Var] -> [Var] -> [String] -> String
writeNetlist cmps vs other_outputs =
    let outputs = map label vs in
    let filter = remove_double $ map label $ vs ++ cmps in
    -- let filter = id in
    let usedv = filter $ cmps ++ vs in
    let (_,inputs,vars,eqs) = foldl (flip rdfs) ([],[],[],[]) usedv in
       "INPUT "    ++ sepBy ", " id       inputs
    ++ "\nOUTPUT " ++ sepBy ", " id       (outputs ++ other_outputs)
    ++ "\nVAR "    ++ sepBy ", " show_var vars
    ++ "\nIN\n"    ++ sepBy "\n" id       eqs
 where rdfs v@(l,s,_) x@(sns,ai,av,ae) = if elem l sns then x
                                         else if is_dummy v then (sns,ai,av,ae)
                                         else dfs v (l:sns,ai,(l,s) : av, ae)
       dfs (l,s,Einput) (sns,ai,av,ae) = (sns,l:ai,av,ae)
       dfs (l,s,Edummy) (sns,ai,av,ae) = (sns,ai,av,ae)
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

runVM :: [String] -> VarMonad a -> a
runVM g m = case S.evalStateT m g of
    Right x -> x
    Left  e -> error e

singleton :: Var -> VarMonad [Var]
singleton v = return [v]
outputs :: [VarMonad Var] -> VarMonad [Var]
outputs vs = M.sequence vs

create :: (Int8,TVar) -> VarMonad Var
create (s,t) = nlabel >>= \l -> return (l,s,t)

input :: String -> Int8 -> VarMonad Var
input l s = return (l,s,Einput)

copy :: Var -> VarMonad Var
copy v = create (size v,Earg v)

reg :: Var -> VarMonad Var
reg v = create (size v, Ereg $ label v)

notv :: Var -> VarMonad Var
notv v = create (size v, Enot v)

(&:) :: Var -> Var -> VarMonad Var
v1 &: v2 = if n1 == n2 then create (n1, Eand v1 v2)
           else fail $ "Anding differently sized variables " ++ label v1 ++ " and " ++ label v2
 where n1 = size v1
       n2 = size v2

(|:) :: Var -> Var -> VarMonad Var
v1 |: v2 = if n1 == n2 then create (n1, Eor v1 v2)
           else fail $ "Oring differently sized variables " ++ label v1 ++ " and " ++ label v2
 where n1 = size v1
       n2 = size v2

(^:) :: Var -> Var -> VarMonad Var
v1 ^: v2 = if n1 == n2 then create (n1, Exor v1 v2)
           else fail $ "Xoring differently sized variables " ++ label v1 ++ " and " ++ label v2
 where n1 = size v1
       n2 = size v2

(!:) :: Var -> Var -> VarMonad Var
v1 !: v2 = if n1 == n2 then create (n1, Enand v1 v2)
           else fail $ "Nanding differently sized variables " ++ label v1 ++ " and " ++ label v2
 where n1 = size v1
       n2 = size v2

(<:) :: Var -> (Var, Var) -> VarMonad Var
v1 <: (v2,v3) = if n1 /= 1 then fail "Muxing on nap"
              else if n2 /= n3 then fail $ "Muxing two differently sized variables when selecting " ++ label v2 ++ " and " ++ label v3
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
                else if i2 >= size v then fail $ "Too big bound on splice on " ++ label v
                else create (i2 - i1 + 1, Eslice i1 i2 v)

(@:) :: Var -> Int8 -> VarMonad Var
v @: i = if i >= size v then fail $ "Select indice too big for " ++ label v
                                    ++ " : " ++ show i ++ " >= " ++ (show $ size v)
         else create (1, Eselect i v)

constV :: Int8 -> Int -> VarMonad Var
constV s i = create (s, Econst i)

dummy :: String -> Int8 -> Var
dummy nm s = (nm, s, Edummy)

type Netlist = ([Var],[Var],[String]) -- to be calculated variables,out varables names, out names for debug

putNetlist :: Netlist -> IO ()
putNetlist (a,b,c) = putStrLn $ writeNetlist a b c
