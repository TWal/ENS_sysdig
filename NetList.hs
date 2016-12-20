
module NetList where
import Data.Int
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad as M

-- A variable is a size and a computing tree
-- The string uniquely identify the var
type Var = (String,Int8,TVar)
data TVar =
    Einput
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

writeNetlist :: [Var] -> String
writeNetlist vs =
    let outputs = map label vs in
    let (_,inputs,vars,eqs) = foldl (flip rdfs) ([],[],[],[]) vs in
       "INPUT "    ++ sepBy ", " id       inputs
    ++ "\nOUTPUT " ++ sepBy ", " id       outputs
    ++ "\nVAR "    ++ sepBy ", " show_var vars
    ++ "\nIN\n"    ++ sepBy "\n" id       eqs
 where rdfs v@(l,s,_) x@(sns,ai,av,ae) = if elem l sns then x
                                         else dfs v (l:sns,ai,(l,s):av,ae)
       dfs (l,s,Einput) (sns,ai,av,ae) = (sns,l:ai,av,ae)
       dfs (l,s,Earg v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = " ++ label v):ae)
       dfs (l,s,Econst i) (sns,ai,av,ae) =
           (sns,ai,av,(l ++ " = " ++ show i):ae)
       dfs (l,s,Ereg v) (sns,ai,av,ae) = rdfs v
           (sns,ai,av,(l ++ " = REG " ++ label v):ae)
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
           (sns,ai,av,(l ++ " = EMUX " ++ label v3 ++ " " ++ label v2 ++ " " ++ label v1):ae)
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
           (sns,ai,av,(l ++ " = CONCAT " ++ label v1 ++ " " ++ label v2):ae)
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
copy (_,s,t) = create (s,t)

reg :: Var -> VarMonad Var
reg v = create (size v, Ereg v)

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
v !!: (i1,i2) = create (i2 - i1 + 1, Eslice i1 i2 v)

(@:) :: Var -> Int8 -> VarMonad Var
v @: i = create (1, Eselect i v)

constV :: Int8 -> Int -> VarMonad Var
constV s i = create (s, Econst i)

