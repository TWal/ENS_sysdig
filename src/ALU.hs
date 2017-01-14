module ALU where
import NetList
import Flags
import Registers
import Utility
import Debug.Trace
import Data.Int

-- in all ALU c , o ,s , ... designate flags
alu :: Var -> Var -> Var -> Var -> (Var,Var,(Var,Var,Var,Var,Var),Var)
alu bin func op1 op2 = runVM (make_gen "alu") $ do
  binres <- return $ binGlob func op1 op2
  unres <- return $ unGlob func op1 op2
  (c,o,out) <- bin <::: (binres,unres)
  flags <- return $ setFlags c o out
  func3 <- func @: 3
  nfunc3 <- notv func3
  wen <- nfunc3 |: bin
  movsf <- select4_bit 1 func

  ismovs <- movsf &: bin
  fen <- notv ismovs
  return (out,wen,flags,fen)

simpleUnInt :: Int8 -> Var -> Var -> VarMonad (Var,Var)
simpleUnInt 1 c inp = do
  i <- inp @: 0
  out <- c ^: i
  co <- c &: i
  return (co,out)

simpleUnInt n c inp = do
  (ci,oend) <- simpleUnInt (n-1) c inp
  i <-  inp @: (n-1)
  obeg <- ci ^: i
  co <- ci &: i
  out <- obeg -: oend
  return (co,out)

simpleUn :: Var -> Var -> (Var,Var,Var)
simpleUn uncode op1 = runVM (make_gen "simpleUn") $ do
  l <- mapM (\(i,c) -> constV 4 c >>= \v -> return (i,v)) [
    (binaryToInt8 "00", binaryToInt "0010"),--NOT
    (binaryToInt8 "01", binaryToInt "1111"),--DECR
    (binaryToInt8 "10", binaryToInt "1100"),--INCR
    (binaryToInt8 "11", binaryToInt "1110")]--NEG
  bits <- long_select2 uncode l
  overen <- bits @: 3
  c <-  bits @: 2
  ix <- bits @: 1
  ox <-  bits @: 0
  ri <-  op1 ^-: ix
  (co,ro) <- simpleUnInt 16 c ri
  out <- ro ^-: ox

  --overflow flags
  i15 <- ri @: 15
  o15 <- ro @: 15
  ni15 <- notv i15
  over <- o15 &: ni15
  o <- overen &: over

  return (co,o,out)

--------------------SIMPLE BINARY OP--------------------------
simpleBinInt :: Int8 -> Var -> Var -> Var -> VarMonad (Var,Var)
simpleBinInt 1 c xab andab = do
  xabbit <- xab @: 0
  andabbit <- andab @: 0
  ario <- renameVM "ario0" $ c ^: xabbit
  cc <- c &: xabbit
  co <- renameVM "co0" $ andabbit |: cc
  return (co,ario)

simpleBinInt n c xab andab = do
  (ci,arioend) <- simpleBinInt (n-1) c xab andab
  xabbit <- xab @: (n-1)
  andabbit <- andab @: (n-1)
  ario <- renameVM ("ario" ++ (show $ n-1)) $ ci ^: xabbit
  cc <- ci &: xabbit
  co <- renameVM ("co" ++  (show $n-1)) $ andabbit |: cc
  out <- ario -: arioend
  return(co,out)

simpleBin :: Var -> Var -> Var -> (Var,Var,Var)
simpleBin bincode a b = runVM (make_gen "simpleBin") $ do
  l <- mapM (\(i,c) -> constV 7 c >>= \v -> return (i,v)) [
    (binaryToInt8 "1000", binaryToInt "0000001"),--ADD
    (binaryToInt8 "1001", binaryToInt "0000101"),--ADDC
    (binaryToInt8 "1010", binaryToInt "0101001"),--SUB
    (binaryToInt8 "1011", binaryToInt "0101101"),--SUBC
    (binaryToInt8 "1100", binaryToInt "0000010"),--AND
    (binaryToInt8 "1101", binaryToInt "1110010"),--OR
    (binaryToInt8 "1110", binaryToInt "0000000"),--XOR
    (binaryToInt8 "1111", binaryToInt "0010010")]--NAND
  bits <- renameVM "binbits" $ long_select4 bincode l
  ax <- bits @:6
  bx <- bits @:5
  ox <- bits @:4
  ci <- bits @:3
  cf <- bits @:2
  andb <- bits @:1
  ari <- bits @:0
  ra <- a ^-: ax
  rb <- b ^-: bx
  xab <- renameVM "xab" $ ra ^: rb
  andab <- renameVM "andab" $ ra &: rb
  fc <- return $ get_flag "c"
  cflag <- cf &: fc
  c <- ci ^: cflag
  (co,ario') <- simpleBinInt 16 c xab andab
  let ario = renameV "ario" ario'
  logo <- andb <: (andab,xab)
  ro <- ari <: (ario,logo)
  out <- ro ^-: ox

  --overflow flags
  a15 <- ra @: 15
  b15 <- rb @: 15
  o15 <- ro @: 15
  diffsign <- a15 ^: b15
  diffsignout <- a15 ^: o15
  zero <- constV 1 0
  over <- diffsign <: (zero,diffsignout)
  o <- ari &: over

  return (co,o,out)

----------------------------SHIFTS----------------------------
shiftlInt :: Int -> Var -> Var -> VarMonad Var
shiftlInt n doit inp = do
  slice <- inp !!: (0,15-2^n)
  zero <- constV (2^n) 0
  tmp <- slice -: zero
  doit <: (tmp,inp)

shiftl :: Var -> Var -> VarMonad Var
shiftl inp val = do
  v3 <- val @: 3
  v2 <- val @: 2
  v1 <- val @: 1
  v0 <- val @: 0
  res3 <- shiftlInt 3 v3 inp
  res2 <- shiftlInt 2 v2 res3
  res1 <- shiftlInt 1 v1 res2
  shiftlInt 0 v0 res1

makeArray :: Int8 -> Var -> VarMonad Var
makeArray 1 v = do
  return v

makeArray n v = do
  vend <- makeArray (n-1) v
  v -: vend

shiftrInt :: Int8 -> Var -> Var -> Var -> VarMonad Var
shiftrInt n doit inp fstbit = do
  slice <- inp !!: (2^n,15)
  arr <- makeArray (2^n) fstbit
  tmp <- arr -: slice
  doit <: (tmp,inp)

shiftr :: Var -> Var -> Var -> VarMonad Var
shiftr inp val fstbit = do
  v3 <- val @: 3
  v2 <- val @: 2
  v1 <- val @: 1
  v0 <- val @: 0
  res3 <- shiftrInt 3 v3 inp fstbit
  res2 <- shiftrInt 2 v2 res3 fstbit
  res1 <- shiftrInt 1 v1 res2 fstbit
  shiftlInt 0 v0 res1

simpleShift :: Var -> Var -> Var -> VarMonad Var
simpleShift bincode a b = do
  highb <- b !!: (4,15)
  ishigh <- nap_or highb
  b15 <- b @: 15
  resifhigh <- makeArray 16 b15
  bc0 <- bincode @:0
  bc1 <- bincode @:1
  zero <- constV 1 0
  fstb <- bc1 <: (b15,zero)
  sl <- shiftl a b
  sr <- shiftr a b fstb
  resiflow <- bc0 <:(sl,sr)
  ishigh <: (resifhigh,resiflow)


------------------------------FUSION CODE-----------------------------

binGlob :: Var -> Var -> Var -> (Var,Var,Var)
binGlob code op1 op2 = runVM (make_gen "binGlob") $ do
  binres <- return $ simpleBin code op1 op2
  shiftres <- simpleShift code op1 op2
  code1 <- code @: 1
  code2 <- code @: 2
  code3 <- code @: 3
  code0 <- code @: 0
  isbin <- return code3
  zero <- constV 1 0
  binshiftres <- isbin <::: (binres,(zero,zero,shiftres))
  ismovtmp <- code1 |: code2
  nismov <- ismovtmp |: code3
  ismov <- notv nismov
  ismov <::: ((zero,zero,op1),binshiftres)


unGlob :: Var -> Var -> Var -> (Var,Var,Var)
unGlob code op1 op2 = runVM (make_gen "unGlob") $ do
  simpleunres <- return $ simpleUn code op1
  shiftres <- simpleShift code op1 op2
  code1 <- code @: 1
  code2 <- code @: 2
  code3 <- code @: 3
  code0 <- code @: 0
  isshift <- return code2
  zero <- constV 1 0
  simpleres <- isshift <:::((zero,zero,shiftres),simpleunres)
  isnotSimple <- return code3
  prout <- constV 16 0
  isnotSimple <:::((zero,zero,prout),simpleres)

setFlags :: Var -> Var -> Var -> (Var,Var,Var,Var,Var)
setFlags c o out = runVM (make_gen "setFlags") $ do
  nz <- nap_or out
  z <- notv nz
  np <- out @: 15
  p <- notv np
  sixty <- constV 16 60
  xor60 <- out ^: sixty
  ns <- nap_or xor60
  s <- notv ns
  return (z,c,p,o,s)
