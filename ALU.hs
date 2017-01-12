module ALU where
import NetList
import Flags
import Registers
import Utility

-- in all ALU c , o ,s , ... designate flags

alu bin func op1 op2 = runVM (make_gen "alu") $ do



  return (flags,out)


simpleUnInt 1 c ix ox input = do
  i <- input @: 0
  ri <- i ^: ix
  ro <- c ^: ri
  out <- ro ^: ox
  co <- c &: ri
  return (out,co)

simpleUnInt n c ix ox input = do
  (oend,ci) <- simpleUnInt (n-1) c ix ox input
  i <- input @: (n-1)
  ri <- i ^: ix
  ro <- ci ^: ri
  obeg <- ro ^: ox
  co <- ci &: ri
  out <- obeg -: oend
  return (out,co)

simpleUn uncode op1 = runVM (make_gen "simpleUn") $ do
  l <- mapM (\(i,c) -> constV 3 c >>= \v -> return (i,v)) [
    (0,2),
    (1,7),
    (2,4),
    (3,6)]
  bits <- long_select2 uncode l
  c <- bits @: 2
  ix <- bits @: 1
  ox <- bits @: 0
  simpleUnInt 16 c ix ox op1

--------------------SIMPLE BINARY OP---------------------

simpleBinInt 1 c xab andab = do
  xabb <- xab @: 0
  andabb <- andab @: 0
  ario <- c ^: xabb
  cc <- c &: andabb
  co <- andabb |: cc
  return (ario,co)

simpleBinInt n c xab andab = do
  (arioend,ci) <- simpleBinInt (n-1) xab andab
  xabb <- xab @: (n-1)
  andabb <- andab @: (n-1)
  ario <- ci ^: xabb
  cc <- ci &: andabb
  co <- andabb |: ci
  out <- ario -: arioend
  return(out,co)

simpleBin bincode a b = runVM (make_gen "simpleBin") $ do
  l <- mapM (\(i,c) -> constV 7 c >>= \v -> return (i,v)) [
    (8,1),
    (9,5),
    (10,41),
    (11,45),
    (12,114),
    (13,0)
    (14,2)
    (15,18)
    ]
  bits <- long_select4 bincode l
  ax <- bits @:6
  bx <- bits @:5
  ox <- bits @:4
  ci <- bits @:3
  cf <- bits @:2
  and <- bits @:1
  ari <- bits @:0
  ra <- a ^: ax
  rb <- b ^: bx
  xab <- ra ^: rb
  andab <- ra &: rb
  fc <- get_flag "c"
  cflag <- cf &: fc
  c <- ci ^: cflag
  (ario,co) <- simpleBinInt 16 c xab andab
  logo <- and <: (andab,xab)
  ro <- ari <: (ario,logo)
  out <- ro ^: ox

  a15 <- ra @: 15
  b15 <- rb @: 15
  o15 <- ro @: 15
  diffsign <- a15 ^: b15
  diffsignout <- a15 ^: o15
  o <- diffsign <: (0,diffsignout)
  return (out,co,o)

----------------------------SHIFTS----------------------

shiftlInt n doit input = do
  slice <- input !!: (0,15-2^n)
  zero <- constV (2^n) 0
  tmp <- slice -: zero
  doit <: (tmp,input)

shiftl input val = runVM (make_gen "shiftl") $ do
  v3 <- val @: 3
  v2 <- val @: 2
  v1 <- val @: 1
  v0 <- val @: 0
  res3 <- shiftlInt 3 v3 input
  res2 <- shiftlInt 2 v2 res3
  res1 <- shiftlInt 1 v1 res2
  shiftlInt 0 v0 res1

makeArray 1 v = do
  return v

makeArray n v = do
  vend <- makeArray (n-1) v
  v -: vend

shiftrInt n doit input fstbit = do
  slice <- input !!: (2^n,15)
  arr <- makeArray (2^n) fstbit
  tmp <- arr -: slice
  doit <: (tmp,input)

shiftr input val fstbit = runVM (make_gen "shiftr") $ do
  v3 <- val @: 3
  v2 <- val @: 2
  v1 <- val @: 1
  v0 <- val @: 0
  res3 <- shiftrInt 3 v3 input fstbit
  res2 <- shiftrInt 2 v2 res3 fstbit
  res1 <- shiftrInt 1 v1 res2 fstbit
  shiftlInt 0 v0 res1

simpleShift bincode a b = runVM (make_gen "simpleShift") $ do
  highb <- b !!: (4,15)
  ishigh <- nap_or highb
  b15 <- b @: 15
  resifhigh <- makeArray 16 b15
  bc0 <- bincode @:0
  bc1 <- bincode @:1
  fstb <- bc1 <: (b15,0)
  sl <- shiftl a b
  sr <- shiftr a b fstb
  resiflow <- bc0 <:(sl,sr)
  ishigh <: (resifhigh,resiflow)


------------------------------FUSION CODE-------------------

movOver code op1 op2 = runVM (make_gen "movOver") $ do
  binres <- simpleBin code op1 op2
  code1 <- code @: 1
  code2 <- code @: 2
  code3 <- code @: 3
  ismovtmp <- code1 &: code2
  ismov <- ismovtmp &: code3
  ismov <: (op1,binres)


setFlags c o out = runVM (make_gen "setFlags") $ do
  nz <- nap_and out
  z <- notv nz
  np <- out @: 15
  p <- notv np
  sixty <- constV 16 60
  xor60 <- out &: sixty
  ns <- nap_and xor60
  s <- notv ns
  return (z,c,p,o,s)
