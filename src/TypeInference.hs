module TypeInference where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set

import Utils

-- Based on: https://course.ccs.neu.edu/cs4410sp19/lec_type-inference_notes.html

-- Starting type environment
typeEnv :: TypeEnvt
typeEnv = Map.empty

-- Starting type scheme environment
schEnv :: SchemeEnvt
schEnv = Map.fromList [
  ((show Addition), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TNumber))),
  ((show Subtraction), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TNumber))),
  ((show Multiplication), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TNumber))),
  ((show Division), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TNumber))),
  ((show FloorDivision), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TNumber))),
  ((show Remainder), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TNumber))),
  ((show Exponentiation), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TNumber))),
  ((show GreaterThan), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TBoolean))),
  ((show GreaterEqual), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TBoolean))),
  ((show LessThan), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TBoolean))),
  ((show LessEqual), Forall [] (TyArr [TyCon TNumber, TyCon TNumber] (TyCon TBoolean))),
  ((show Equal), Forall ["x"] (TyArr [TyVar "x", TyVar "x"] (TyCon TBoolean))),
  ((show NotEqual), Forall ["x"] (TyArr [TyVar "x", TyVar "x"] (TyCon TBoolean))),
  ((show BoolOr), Forall [] (TyArr [TyCon TBoolean, TyCon TBoolean] (TyCon TBoolean))),
  ((show BoolAnd), Forall [] (TyArr [TyCon TBoolean, TyCon TBoolean] (TyCon TBoolean))),
  ((show Negation), Forall [] (TyArr [TyCon TNumber] (TyCon TNumber))),
  ((show BoolNot), Forall [] (TyArr [TyCon TBoolean] (TyCon TBoolean)))]

-- Infer the types of the variables and functions in the AST and return the type environment
inferTypes :: AST -> TypeEnvt
inferTypes ast = envt
                where
                  (_, (envt, _, _)) = appTST (inferAST ast) (typeEnv, schEnv, 0)

-- Replace the type variable String in the 2nd Type with the 1st Type
substVarType :: String -> Type -> Type -> Type
substVarType var to t@(TyVar v) = if var == v then to else t
substVarType var to (TyArr xs y) = TyArr xs' y'
                                   where
                                     xs' = map (substVarType var to) xs
                                     y' = substVarType var to y
substVarType var to (TyArray e y) = TyArray (substVarType var to e) y                                    
substVarType _ _ t = t

-- Replace the type variable String in the Scheme with the Type
substVarScheme :: String -> Type -> Scheme -> Scheme
substVarScheme var to (Forall xs t) = Forall xs (substVarType var to t)

-- Apply the Subst to the Type
applySubstType :: Subst -> Type -> Type
applySubstType s t@(TyVar v) = case (lookup v s) of
                                 (Just t') -> t'
                                 Nothing -> t
applySubstType s (TyArr xs y) = TyArr xs' y'
                                where
                                  xs' = map (applySubstType s) xs
                                  y' = applySubstType s y
applySubstType s (TyArray e y) = TyArray (applySubstType s e) y
applySubstType _ t = t

-- Apply the Subst to the type environment in the state
applySubstTypeEnvt :: Subst -> TST ()
applySubstTypeEnvt subst =
  TST(\(env, fenv, i) -> let
                           lenv  = Map.toList env
                           lenv' = map (\(var, typ) -> (var, applySubstType subst typ)) lenv
                         in
                           ((), (Map.fromList lenv', fenv, i)))

-- Apply the 1st Subst to the 2nd
applySubstSubst :: Subst -> Subst -> Subst
applySubstSubst l r = map (\(var,typ) -> (var, applySubstType l typ)) r

-- Compose the Substs together
composeSubst :: Subst -> Subst -> Subst
composeSubst l r = l ++ applySubstSubst l r

-- Get the set of free variables that appear in the Type
ftvType :: Type -> Set.Set String
ftvType (TyVar var) = Set.singleton var
ftvType (TyCon _) = Set.empty
ftvType (TyArr args ret) = foldr (\t -> (\ftvs -> Set.union (ftvType t) ftvs)) (ftvType ret) args
ftvType (TyArray t i) = Set.empty
ftvType TyBlank = Set.empty
ftvType Typeless = Set.empty

-- Does the type variable String appear in the Type
occurs :: String -> Type -> Bool
occurs var t = Set.member var (ftvType t)

-- Instantiate an instance of the Scheme by binding type variables
instantiate :: Scheme -> TST Type
instantiate (Forall [] t) = return t
instantiate (Forall (x:xs) t) =
  do
    newT <- freshVar
    let sch' = substVarScheme x newT (Forall xs t)
    instantiate sch'

-- Raise a unification error
unificationError :: String -> String -> TST Subst
unificationError x y = error ("Unification error - unable to unify " ++ x ++ " and " ++ y)

-- Unify the Types and return the Subst for the substitution performed
unify :: Type -> Type -> TST Subst
unify (TyVar v) t =
  if occurs v t && (TyVar v) /= t
  then unificationError v (show t)
  else
    do
      let subst = [(v, t)]
      applySubstTypeEnvt subst
      return subst
unify t (TyVar v) =
  if occurs v t && (TyVar v) /= t
  then unificationError v (show t)
  else
    do
      let subst = [(v, t)]
      applySubstTypeEnvt subst
      return subst
unify (TyCon c1) (TyCon c2) =
  if c1 /= c2
  then unificationError (show c1) (show c2)
  else return []
unify x@(TyArr args1 ret1) y@(TyArr args2 ret2) =
  if (length args1) /= (length args2)
  then unificationError (show x) (show y)
  else
    do
      argsSubst <- unifyLists args1 args2
      retSubst <- unify ret1 ret2
      let substs = composeSubst argsSubst retSubst
      applySubstTypeEnvt substs
      return substs
unify x@(TyArray t1 i1) y@(TyArray t2 i2) =
  if i1 == i2 || i1 < 0 || i2 < 0
  then
    if i1 == i2
    then unify t1 t2 -- unify the element types
    else -- negative length indicates type variable placeholder
      do
        let var = minimum [i1, i2] -- Get the type variable placeholder length
        let len = maximum [i1, i2] -- Get the actual length
        setArrayLength var len -- Replace the placeholder length in the type environment
        unify t1 t2 -- unify the element types
  else unificationError (show x) (show y)
unify x y = unificationError (show x) (show y)

-- Unify the types in each position of the list and return any Subst
unifyLists :: [Type] -> [Type] -> TST Subst
unifyLists (x:xs) (y:ys) =
  do
    subst <- unify x y
    let xs' = map (applySubstType subst) xs
    let ys' = map (applySubstType subst) ys
    listSubst <- (unifyLists xs' ys')
    return (composeSubst listSubst subst)
unifyLists _ _ = return []

-- Infer the Type of the AST
inferAST :: AST -> TST Type
inferAST (Number n) = return (TyCon TNumber)
inferAST (Name n) =
  do
    mVarT <- getMType n
    case mVarT of
      (Just t) -> return t
      Nothing -> error (n ++ " referenced before assignment")
inferAST (String s) = return (TyCon TString)
inferAST (Boolean b) = return (TyCon TBoolean)
inferAST Nil = return (TyCon TNil)
inferAST (Type t) = return t
inferAST (Statements xs) = case xs of
                             [] -> return Typeless
                             (x:xs) -> do
                                         inferAST x
                                         inferAST (Statements xs)
                                         return Typeless
inferAST (Assignment (Name var) (Type t) val) =
  do
    newT <- freshVar
    mVarT <- getMType var
    let varT = case mVarT of
                 (Just t') -> if (t' == t) || (t == TyBlank)
                              then t'
                              else error (var ++ " has type " ++ (show t') ++
                                          " not type " ++ (show t))
                 Nothing -> if t == TyBlank then newT else t
    addEnv var varT
    valT <- inferAST val
    unify varT valT
    return Typeless
inferAST (Assignment (ArrayIndex (Name var) ind) (Type t) val) =
  do
    mVarT <- getMType var
    newT <- freshVar
    let
      varT = case mVarT of
               (Just t) -> t
               _ -> error ("Unable to get type of " ++ var)
      varT' = case varT of
                a@(TyArray _ _) -> a
                (TyVar _) -> let newT' = case newT of (TyVar v) -> v
                             -- Get a new array type with placeholder length
                             in (TyArray newT (-(read newT' :: Int)))
                _ -> error (var ++ " isn't an array")
      varIndT = case varT' of (TyArray v _) -> v
    unify varT varT'
    valT <- inferAST val
    unify varIndT valT
    indT <- inferAST ind
    unify indT (TyCon TNumber)
    return Typeless
inferAST (BinOp op l r) =
  do
    opSch <- getOpSch (show op)
    opT <- instantiate opSch
    lT <- inferAST l
    rT <- inferAST r
    retT <- freshVar
    let newArrT = (TyArr [lT, rT] retT)
    subst <- unify newArrT opT
    return (applySubstType subst retT)
inferAST (UnOp op x) =
  do
    opSch <- getOpSch (show op)
    opT <- instantiate opSch
    xT <- inferAST x
    retT <- freshVar
    let newArrT = (TyArr [xT] retT)
    subst <- unify newArrT opT
    return (applySubstType subst retT)
inferAST (IfThen c b) =
  do
    cT <- inferAST c
    unify cT (TyCon TBoolean)
    inferAST b
    return Typeless
inferAST (IfElse c t e) =
  do
    cT <- inferAST c
    unify cT (TyCon TBoolean)
    inferAST t
    inferAST e
    return Typeless
inferAST (WhileLoop c b) =
  do
    cT <- inferAST c
    unify cT (TyCon TBoolean)
    inferAST b
    return Typeless
inferAST (RepeatUntil b c) =
  do
    inferAST b
    cT <- inferAST c
    unify cT (TyCon TBoolean)
    return Typeless
inferAST (ForLoop i@(Assignment _ _ val) c s b) =
  do
    inferAST i
    vT <- inferAST val
    unify vT (TyCon TNumber)
    cT <- inferAST c
    unify cT (TyCon TNumber)
    sT <- inferAST s
    unify sT (TyCon TNumber)
    inferAST b
    return Typeless
inferAST (Function (Name n) args b) =
  do
    mNT <- getMType n
    case mNT of
      (Just _) -> error (n ++ " already declared")
      Nothing ->
        do
          argTs <- inferASTs args
          retT <- freshVar
          addEnv n (TyArr argTs retT)
          inferAST b
          retTs <- getRetTs b
          let retTsTl = if (length retTs) <= 1 then [] else (tail retTs)
          subst <- unifyLists retTs retTsTl
          let retT' = case retTs of
                        [] -> (TyCon TNil)
                        _  -> applySubstType subst (head retTs)
          argTs' <- inferASTs args
          addEnv n (TyArr argTs' retT')
          return (TyArr argTs' retT')
inferAST (Return e) = inferAST e
inferAST (Arg (Name n) (Type t)) =
  do
    mNT <- getMType n
    case mNT of
      (Just t') ->
        do
          addEnv n t'
          addEnv ("-r" ++ n) t'
          return t'
      _ ->
        do
          newT <- freshVar
          let t' = case t of
                    TyBlank -> newT
                    _ -> t
          addEnv n t'
          addEnv ("-r" ++ n) t'
          return t'
inferAST (Call (Name n) args) =
  do 
    argTs <- inferASTs args
    mNT <- getMType n
    let
      nT = case mNT of
             (Just t) -> t
             _ -> error (n ++ " referenced before assignment")
      (nArgTs, nRetT) = case nT of
                          (TyArr as r) -> (as, r)
                          _ -> error (n ++ " isn't a function")
    unifyLists argTs nArgTs
    if (length args) == (length nArgTs)
    then return (nRetT) 
    else error ("Call to " ++ n ++
                " wasn't provided with the correct number of arguments")
inferAST Break = return Typeless
inferAST (Print e) = inferAST e >> return Typeless
inferAST (ArrayIndex (Name n) i) =
  do
    mNT <- getMType n
    newT <- freshVar
    let
      nT = case mNT of
             (Just t) -> t
             _ -> error (n ++ " referenced before assignment")
      nT' = case nT of
              (TyArray t _) -> nT
              (TyVar _) -> let newT' = case newT of (TyVar v) -> v
                           -- Get a new array type with placeholder length
                           in (TyArray newT (-(read newT' :: Int)))
              _ -> error (n ++ " isn't an array")
      vT = case nT' of (TyArray t _) -> t
    unify nT nT'
    iT <- inferAST i
    unify iT (TyCon TNumber)
    return vT
inferAST (ArrayInit vs) =
  if vs /= []
  then
    do
      vsTs <- inferASTs vs
      subst <- unifyLists vsTs (tail vsTs)
      let vT = applySubstType subst (head vsTs)
      return (TyArray vT (length vs))
  else
    do
      t <- freshVar
      return t
inferAST _ = error "Unable to infer type"

-- Infer the types of a list of ASTs
inferASTs :: [AST] -> TST [Type]
inferASTs [] = return []
inferASTs (x:xs) =
  do
    t <- inferAST x
    ts <- inferASTs xs
    return (t:ts)

-- Get types of list of arguments
getArgTs :: [AST] -> TST [Type]
getArgTs [] = return []
getArgTs ((Arg (Name n) _):as) =
  do
    mT <- getMType n
    case mT of
      (Just t) ->
        do
          ts <- getArgTs as
          return (t:ts)
      Nothing -> error ("Unable to get type of argument " ++ n)

-- Get types of return statements in AST
getRetTs :: AST -> TST [Type]
getRetTs (Statements []) = return []
getRetTs (Statements (x:xs)) =
  do
    ts1 <- getRetTs x
    ts2 <- getRetTs (Statements xs)
    return (ts1 ++ ts2)
getRetTs (IfThen _ b) = getRetTs b
getRetTs (IfElse _ i e) =
  do
    iTs <- getRetTs i
    eTs <- getRetTs e
    return (iTs ++ eTs)
getRetTs (WhileLoop _ b) = getRetTs b
getRetTs (ForLoop _ _ _ b) = getRetTs b
getRetTs (RepeatUntil b _) = getRetTs b
getRetTs (Return e) =
  do
    t <- inferAST e
    return [t]
getRetTs _ = return []