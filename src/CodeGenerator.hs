module CodeGenerator where

import Text.Printf
import qualified Data.Map as Map

import TypeInference
import Utils

-- Generate the LLVM IR code for the AST using the TypeEnvt
-- Returns variables, strings and functions, and main body
generateCode :: AST -> TypeEnvt -> ([String], [String], [String])
generateCode (Statements xs) envt =
  let
    vars = generateVars (Map.toList envt)
    (c, s) = (appGST (generate xs) (GenState { reg = 0
                                             , lbl = 1
                                             , env = envt
                                             , code = []}))
  in
    (vars, checkBreak (code s), checkBreak c)

-- Generate the LLVM IR code for the ASTs
generate :: [AST] -> GST [String]
generate [] = return []
generate (a:as) =
  do
    (_, xs) <- generate' a
    ys <- generate as
    return (xs ++ ys)

-- Generate the LLVM IR code for the AST
-- Returns the register containing the result and the lines of code
generate' :: AST -> GST (String, [String])
generate' (Register i) =
  do
    r <- freshReg
    let
      l = loadPrint (TyCon TNumber) ("%r" ++ (show i)) :: String
      a = assignPrint r l :: String
    return (r, [a])
generate' (String s) = addString s >> return (getStringRegister s, [])
generate' (Number i) = return (show i, [])
generate' (Boolean b) = if b then return ("1", []) else return ("0", [])
generate' (Nil) = return ("null", [])
generate' (Name n) =
  do
    t <- getType n
    r <- freshReg
    let
      n' = getRegister n
      l = loadPrint t n' :: String
      a = assignPrint r l
    return (r, [a])
generate' (Assignment (Register i) (Type t) x) =
  do
    (x', xs) <- generate' x
    let s = storePrint t x' ("%r" ++ (show i))
    return ("", (xs ++ [s]))
generate' (Assignment (ArrayIndex (Name x) i) _ y) =
  do
    xTC <- getTypeConstant x
    (i', is) <- generate' (BinOp Subtraction i (Number 1.0))
    (y', ys) <- generate' y
    r <- freshReg
    ind <- freshReg
    let
      x' = getRegister x
      c = assignPrint ind ("fptoui double " ++ i' ++ " to i32")
      p = getElemPtrPrint xTC x' ind :: String
      a = assignPrint r p
      s = storePrint (TyCon xTC) y' r
    return ("", is ++ ys ++ [c] ++ [a] ++ [s])
generate' (Assignment (Name x) _ y@(String str)) =
  do
    (y', ys) <- generate' y
    r <- freshReg
    let
      x' = getRegister x
      g = getElemPtrStringPrint y' :: String
      a = assignPrint r g :: String
      s = storePrint (TyCon TString) r x' :: String
    return ("", (ys ++ [a] ++ [s]))
generate' (Assignment (Name x) _ y) =
  do
    t <- getType x
    (y', ys) <- generate' y
    let
      x' = getRegister x
      s = storePrint t y' x' :: String
    return ("", (ys ++ [s]))
generate' (BinOp op x y) =
  do
    (x', xs) <- generate' x
    (y', ys) <- generate' y
    r <- freshReg
    t <- inferTypeConstant x
    let
      o = case op of
              Equal -> equalPrint t x' y' :: String
              NotEqual -> notEqualPrint t x' y' :: String
              _ -> binOpPrint op x' y' :: String
      a = assignPrint r o :: String
    return (r, xs ++ ys ++ [a])
generate' (UnOp op x) =
  do
    (x', xs) <- generate' x
    r <- freshReg
    let
      o = unOpPrint op x' :: String
      s = assignPrint r o :: String
    return (r, xs ++ [s])
generate' (Statements xs) =
  do
    xs' <- generate xs
    return ("", xs')
generate' (IfThen x y) =
  do
    (x', xs) <- generate' x
    (_, ys) <- generate' y
    (l0, r0) <- freshLbl
    (l1, r1) <- freshLbl
    let
      cb = condBranchPrint x' r0 r1 :: String
      b = branchPrint r1 :: String
    return ("", xs ++ [cb] ++ [l0] ++ ys ++ [b] ++ [l1])
generate' (IfElse x y z) =
  do
    (x', xs) <- generate' x
    (_, ys) <- generate' y
    (_, zs) <- generate' z
    (l0, r0) <- freshLbl
    (l1, r1) <- freshLbl
    (l2, r2) <- freshLbl
    let
      cb = condBranchPrint x' r0 r1 :: String
      b = branchPrint r2 :: String
    return ("", xs ++ [cb] ++ [l0] ++ ys ++ [b] ++ [l1] ++ zs ++ [b] ++ [l2])
generate' (WhileLoop x y) =
  do
    (x', xs) <- generate' x
    (_, ys) <- generate' y
    (l0, r0) <- freshLbl
    (l1, r1) <- freshLbl
    (l2, r2) <- freshLbl
    let
      b = branchPrint r0 :: String
      cb =  condBranchPrint x' r1 r2
      ls = [b] ++ [l0] ++ xs ++ [cb] ++ [l1] ++ ys ++ [b] ++ [l2]
      brk = branchPrint r2 :: String
    return ("", setBreak brk ls)
generate' (RepeatUntil b c) =
  do
    (_, bs) <- generate' b
    (c', cs) <- generate' c
    (l0, r0) <- freshLbl
    (l1, r1) <- freshLbl
    let
      b = branchPrint r0 :: String
      cb = condBranchPrint c' r1 r0:: String
      ls = [b] ++ [l0] ++ bs ++ cs ++ [cb] ++ [l1]
      brk = branchPrint r1 :: String
    return ("", setBreak brk ls)
generate' (ForLoop i@(Assignment v _ _) c s b) =
  do
    m <- freshReg
    let mi = read (drop 2 m) :: Int
    (_, is) <- generate' i
    (_, ms) <- generate' (IfElse
                            (BinOp LessThan s (Number 0))
                            (Assignment (Register mi) (Type (TyCon TNumber)) (Number (-1)))
                            (Assignment (Register mi) (Type (TyCon TNumber)) (Number 1))) 
    (c', cs) <- generate' (BinOp LessEqual 
                             (BinOp Multiplication (Register mi) v)
                             (BinOp Multiplication (Register mi) c))
    (_, ss) <- generate' (Assignment v (Type (TyCon TNumber)) (BinOp Addition v s))
    (_, bs) <- generate' b
    (l0, r0) <- freshLbl
    (l1, r1) <- freshLbl
    (l2, r2) <- freshLbl
    let
      m' = m ++ " = alloca double"
      b = branchPrint r0 :: String
      cb = condBranchPrint c' r1 r2
      ls = ([m'] ++ is ++ ms ++ [b] ++ [l0] ++ cs ++ [cb] ++ [l1] ++ bs ++ ss ++ [b] ++ [l2])
      brk = branchPrint r2 :: String
    return ("", setBreak brk ls)
generate' (Function (Name n) args b) =
  do
    fT <- getType n
    let (argsTs, retT) = case fT of (TyArr ats rt) -> (ats, rt)
    if tyVarElem (retT:argsTs) -- If there is a type variable in the function type
    then return ("", []) -- Then this function hasn't been used so don't generate it
    else
      do
        argsStr <- getArgsString args argsTs
        ps <- generateParameters args
        let
          argsNs = map (\(Arg (Name n) _) -> n) args 
          b' = renamePrefix argsNs "-r" b
        (_, bs) <- generate' b'
        let
          n' = getRegister n
          retTC = case retT of (TyCon c) -> c
          argsStr' = if length argsStr > 0
                     then (removeComma argsStr)
                     else argsStr
          f = functionPrint (show retTC) n' argsStr' :: String
          r = if retTC == TNil
              then [returnPrint retTC "null" :: String]
              else []
        addFunctionCode ([f] ++ ps ++ bs ++ r ++ ["}"])
        return ("", [])
generate' (Return e) =
  do
    (e', es) <- generate' e
    t <- inferTypeConstant e
    let r = returnPrint t e' :: String
    return ("", es ++ [r])
generate' (Call (Name n) args) =
  do
    t <- getType n
    let (argsTs, retT) = case t of (TyArr s r) -> (s, r)
    (argsStr, as) <- getArgsTypesString args argsTs
    r <- freshReg
    let
      n' = getRegister n
      argsStr' = if length argsStr > 0
                 then (removeComma argsStr)
                 else argsStr
      c = callPrint (show retT) n' argsStr' :: String
      a = assignPrint r c :: String
    return (r, as ++ [a])
generate' Break = return ("", [breakPlaceholder])
generate' (Print e) =
  do
    (e', es) <- generate' e
    t <- inferTypeConstant e
    let
      p = case t of
            TNil -> nilPrint :: String
            TBoolean -> booleanPrint e' :: String
            TNumber -> numberPrint e' :: String
            TString -> stringPrint e' :: String
    return ("", es ++ [p])
generate' (ArrayIndex (Name n) i) =
  do
    nTC <- getTypeConstant n
    (i', is') <- generate' (BinOp Subtraction i (Number 1.0))
    ind <- freshReg
    ptr <- freshReg
    r <- freshReg
    let
      n' = getRegister n
      a = assignPrint ind ("fptoui double " ++ i' ++ " to i32")
      p = getElemPtrPrint nTC n' ind :: String
      b = assignPrint ptr p
      l = loadPrint (TyCon nTC) ptr :: String
      c = assignPrint r l :: String
    return (r, is' ++ [a] ++ [b] ++ [c])
generate' (ArrayInit vs) =
  do
    case vs of
      [] -> return ("zeroinitializer", [])
      vs -> do
              (rs, cs) <- generateArrayInit vs
              t <- inferTypeConstant (head vs) 
              let
                a = case rs of
                      [x] -> "[" ++ (show t) ++ " " ++ (head rs) ++ "]"
                      (x:xs) -> "[" ++ (show t) ++ " " ++ (head rs) ++ (arrayPrint t (tail rs))
              return (a, cs)
generate' x = error ("Unable to generate code for " ++ (show x))

-- Generate LLVM IR code for allocating memory for variables
generateVars :: [(String, Type)] -> [String]
generateVars [] = []
generateVars ((d, t):ds) =
  case (head d) of
    '-' -> generateVars ds
    _   -> case t of
            (TyArr _ _) -> generateVars ds
            (TyCon c) -> 
              (assignPrint
                ('@':d)
                ("global " ++ (show c) ++ " " ++ init) :: String):generateVars ds
               where
                 init = case c of
                          TString -> "null"
                          TNumber -> "0.0"
                          TNil -> "null"
                          TBoolean -> "0"
            (TyArray _ _) ->
              (assignPrint
                ('@':d)
                ("global " ++ (show t) ++ " zeroinitializer") :: String):generateVars ds
            _ -> error ("Unable to get type of " ++ d)

-- Generate the LLVM IR code to allocate memory for function parameters
-- allowing parameters to be mutable
generateParameters :: [AST] -> GST [String]
generateParameters [] = return []
generateParameters ((Arg (Name n) _):as) =
  do
    t <- getType n
    vs <- generateParameters as
    let
      n' = getRegister n
      a = assignPrint ("%-r" ++ n) ("alloca " ++ (show t)) :: String
      s = storePrint t n' ("%-r" ++ n) :: String
    return ([a] ++ [s] ++ vs)

-- Generate the LLVM IR code for an array initialisation
-- Returns the registers of the initial values, and the code to get the values
generateArrayInit :: [AST] -> GST ([String], [String])
generateArrayInit [] = return ([], [])
generateArrayInit (x:xs) =
  do
    (x', xs') <- generate' x
    (y, ys) <- generateArrayInit xs
    return (x':y, xs'++ys)

-- Get the PrintfType for assignment
assignPrint :: PrintfType r => r
assignPrint = printf "%s = %s"

-- Get the PrintfType for storing a value of Type at a pointer
storePrint :: PrintfType r => Type -> r
storePrint t = printf ("store " ++ (show t) ++ " %s, " ++ (show t) ++ "* %s")

-- Get the PrintfType for loading a value of Type at a pointer
loadPrint :: PrintfType r => Type -> r
loadPrint t = printf "load %s, %s* %s" (show t) (show t)

-- Get the PrintfType for the BinaryOperator
binOpPrint :: PrintfType r => BinaryOperator -> r
binOpPrint Addition = printf "fadd double %s, %s"
binOpPrint Subtraction = printf "fsub double %s, %s"
binOpPrint Multiplication = printf "fmul double %s, %s"
binOpPrint Division = printf "fdiv double %s, %s"
binOpPrint FloorDivision = printf "call double @-ffdiv(double %s, double %s)"
binOpPrint Remainder = printf "frem double %s, %s"
binOpPrint Exponentiation = printf "call double @llvm.pow.f64(double %s, double %s)"
binOpPrint GreaterThan = printf "fcmp ogt double %s, %s"
binOpPrint GreaterEqual = printf "fcmp oge double %s, %s"
binOpPrint LessThan = printf "fcmp olt double %s, %s"
binOpPrint LessEqual = printf "fcmp ole double %s, %s"
binOpPrint BoolOr = printf "or i1 %s, %s"
binOpPrint BoolAnd = printf "and i1 %s, %s"

-- Get the PrintfType for the equals operator given the operand TypeConstant
equalPrint :: PrintfType r => TypeConstant -> r
equalPrint t = case t of
                 TNil -> printf "icmp eq i1* %s, %s"
                 TBoolean -> printf "icmp eq i1 %s, %s"
                 TNumber -> printf "fcmp oeq double %s, %s"
                 TString -> printf "icmp eq i8* %s, %s"

-- Get the PrintfType for the not equals operator given the operand TypeConstant
notEqualPrint :: PrintfType r => TypeConstant -> r
notEqualPrint t = case t of
                    TNil -> printf "icmp ne i1* %s, %s"
                    TBoolean -> printf "icmp ne i1 %s, %s"
                    TNumber -> printf "fcmp one double %s, %s"
                    TString -> printf "icmp ne i8* %s, %s"

-- Get the PrintfType for the UnaryOperator
unOpPrint :: PrintfType r => UnaryOperator -> r
unOpPrint Negation = printf "fsub double 0.0, %s"
unOpPrint BoolNot = printf "icmp eq i1 0, %s"

-- Get the PrintfType for unconditional branching
branchPrint = printf "br label %s"

-- Get the PrintfType for conditional branching
condBranchPrint :: PrintfType r => r
condBranchPrint = printf "br i1 %s, label %s, label %s"

-- Get the PrintfType for getting a pointer to a string
getElemPtrStringPrint :: PrintfType r => r
getElemPtrStringPrint = printf "getelementptr i8*, i8** %s, i32 0"

-- Get the PrintfType for getting a pointer to an element in an array
getElemPtrPrint :: PrintfType r => TypeConstant -> r
getElemPtrPrint t = printf "getelementptr %s, %s* %s, i32 %s" (show t) (show t)

-- Get the PrintfType value for tail an array
arrayPrint :: TypeConstant -> [String] -> String
arrayPrint t [] = "]" 
arrayPrint t (x:xs) = ", " ++ (show t) ++ " " ++ x ++ (arrayPrint t xs)

-- Get the PrintfType for the start of a function
functionPrint :: PrintfType r => r
functionPrint = printf "define %s %s(%s) {"

-- Get the PrintfType for a returning a TypeConstant
returnPrint :: PrintfType r => TypeConstant -> r
returnPrint t = printf "ret %s %s" (show t)

-- Get the PrintfType for a function call
callPrint :: PrintfType r => r
callPrint = printf "call %s %s(%s)"

-- Get the PrintfType for printing nil
nilPrint :: String
nilPrint = "call i32 (i8*,...) @printf(i8* @-nil)"

-- Get the PrintfType for printing a boolean
booleanPrint :: PrintfType r => r
booleanPrint = printf "call void @-printBool(i1 %s)"

-- Get the PrintfType for printing a number
numberPrint :: PrintfType r => r
numberPrint = printf "call i32 (i8*,...) @printf(i8* @-formatDouble, double %s)"

-- Get the PrintfType for printing a string
stringPrint :: PrintfType r => r
stringPrint = printf "call i32 (i8*,...) @printf(i8* @-formatString, i8* %s)"

-- Get the arguments String to use with functionPrint
getArgsString :: [AST] -> [Type] -> GST String
getArgsString _ [] = return ""
getArgsString [] _ = return ""
getArgsString ((Arg (Name n) _):xs) (t:ts) =
  do
    let
      n' = getRegister n
      x' = (show t) ++ " " ++ n' ++ ", "
    xs' <- getArgsString xs ts
    return (x' ++ xs')

-- Get the arguments and types String to use with callPrint
getArgsTypesString :: [AST] -> [Type] -> GST (String, [String])
getArgsTypesString [] [] = return ("", [])
getArgsTypesString (x:xs) (t:ts) =
  do
    (x', xs') <- generate' x 
    (y, ys) <- getArgsTypesString xs ts
    let s = (show t) ++ " " ++ x' ++ ", "
    return (s++y, xs' ++ ys)

-- Remove the final comma and space at the end of an arguments or arguments
-- and types string
removeComma :: String -> String
removeComma = reverse . (drop 2) . reverse

-- Check if there are any break place holders left, as it indicates that break
-- has been used outside a loop
checkBreak :: [String] -> [String]
checkBreak [] = []
checkBreak (x:xs) = if x == breakPlaceholder then
                    error "break must be used inside a loop"
                    else x:(checkBreak xs)
                              
-- Set break placeholders to the branch instruction
setBreak :: String -> [String] -> [String]
setBreak _ [] = []
setBreak br (x:[]) = if x == breakPlaceholder
                     then [br]
                     else [x]
setBreak br (x:n:xs) = if x == breakPlaceholder
                       then
                         if (take 2 n) == "br" -- if the next instruction is a branch
                         then br:(setBreak br xs) -- skip it
                         else
                           if n == breakPlaceholder -- if the next instruction is a break
                           then setBreak br (n:xs) -- skip it
                           else br:(setBreak br (n:xs))
                       else x:(setBreak br (n:xs))


-- Is a type variable an element of the list of types
tyVarElem :: [Type] -> Bool
tyVarElem [] = False
tyVarElem ((TyVar _):_) = True
tyVarElem (_:xs) = False || (tyVarElem xs)

-- Get the register of the variable or function
getRegister :: String -> String
getRegister "" = error "No register provided to getRegister"
getRegister x = case (head x) of
                  '-' -> '%':x
                  _   -> '@':x

-- Infer the TypeConstant of the AST
inferTypeConstant :: AST -> GST TypeConstant
inferTypeConstant ast =
  do
    envt <- getEnvGST
    let
      (t, _) = appTST (inferAST ast) (envt, schEnv, 0)
      tc = case t of (TyCon c) -> c
    return tc