module Parser where

import Control.Applicative
import Data.Char

import Utils

-- Parse a list of Tokens and return the corresponding AST
parseTokens :: [Token] -> AST
parseTokens ts = case (parse pBlock ts) of
                   (Just (ast, l)) -> if l == [] then ast else error "Invalid syntax"
                   Nothing -> error "Unable to parse tokens"

-- Parser for block non-terminal
pBlock :: Parser AST
pBlock =
  do
    m <- many pStat
    (do
      pToken (Kword Ret)
      (do
         e <- pExp
         return (Statements (m ++ [Return e]))
       <|>
       return (Statements [Return Nil]))
     <|>
     return (Statements m))

-- Parser for stat non-terminal
pStat :: Parser AST
pStat =
  do
    pToken (Kword Func) -- function
    n <- pName          -- name
    pToken (Par Open)   -- (
    as <- pArgs         -- args
    pToken (Par Closed) -- )
    b <- pBlock         -- block
    pToken (Kword End)  -- end
    let
      -- Need to rename parameters used inside function to differentiate them
      -- from global variables with the same name
      ns = map (\(Arg (Name n) _) -> n) as -- get all parameter names
      pre = case n of (Name x) -> '-':x++"-" -- prefix for parameters
      as' = map (renamePrefix ns pre) as -- rename parameters
      b' = renamePrefix ns pre b -- rename parameters in body
    return (Function n as' b')
  <|>
  do
    pToken (Kword Prt)
    pToken (Par Open)
    e <- pExp
    pToken (Par Closed)
    return (Print e)
  <|>
  do
    pToken (Kword Do)
    b <- pBlock
    pToken (Kword End)
    return b
  <|> 
  do
    pToken (Kword If)
    cond <- pExp
    pToken (Kword Then)
    b <- pBlock
    elfs <- many pElf
    case elfs of
      [] -> (do -- if else
               els <- pEls
               pToken (Kword End)
               return (IfElse cond b els)
             <|>
             do -- if
               pToken (Kword End)
               return (IfThen cond b))
      xs -> do
              (do -- if elseif else
                 els <- pEls
                 pToken (Kword End)
                 -- Chain elseifs together with the else as the base case
                 let xs' = foldr (\(IfThen c b) -> \x -> IfElse c b x) els xs
                 return (IfElse cond b xs')
               <|>
               do -- if elseif
                 pToken (Kword End)
                 let xs' = case xs of
                             [x] -> x
                             -- Chain elseifs together with the first elseif as
                             -- the base case
                             _ -> foldr (\(IfThen c b) -> \x -> IfElse c b x)
                                    (head xs)
                                    (tail xs)
                 return (IfElse cond b xs'))
  <|>
  do
    pToken (Kword While)
    cond <- pExp
    pToken (Kword Do)
    b <- pBlock
    pToken (Kword End)
    return (WhileLoop cond b)
  <|>
  do
    pToken (Kword Rpt)
    b <- pBlock
    pToken (Kword Utl)
    e <- pExp
    return (RepeatUntil b e)
  <|>
  do
    pToken (Kword For) -- for
    n <- pName         -- name
    asn <- pAsn n      -- asn
    pToken Cma         -- ,
    end <- pExp        -- exp
    stp <- pStp        -- stp 
    pToken (Kword Do)  -- do
    b <- pBlock        -- block 
    pToken (Kword End) -- end
    return (ForLoop asn end stp b)
  <|>
  do
    pToken (Kword Brk)
    return (Break)
  <|>
  do
    n <- pName
    (do
        pToken (Par SqOpen)
        i <- pExp
        pToken (Par SqClosed)
        pAsn (ArrayIndex n i)
     <|>
     pAsn n)
  <|>
  do
    n <- pName
    pToken (Par Open)
    es <- pExplist
    pToken (Par Closed)
    return (Call n es)
  <|>
  do
    pToken SCol
    pStat

-- Parser for args non-terminal
pArgs :: Parser [AST]
pArgs =
  do
    a <- pName
    t <- pTyp
    (do
        as <- many (do
                      pToken Cma
                      pArgs)
        return ((Arg a t):(concat as))
     <|>
     return [Arg a t])
  <|>
  return []

-- Parser for elf non-terminal
pElf :: Parser AST
pElf =
  do
    pToken (Kword ElseIf)
    e <- pExp
    pToken (Kword Then)
    b <- pBlock
    return (IfThen e b)

-- Parser for els non-terminal
pEls :: Parser AST
pEls =
  do
    pToken (Kword Else)
    b <- pBlock
    return b

-- Parser for asn non-terminal
pAsn :: AST -> Parser AST
pAsn n =
  do
    t <- pTyp
    pToken Eq
    e <- pExp
    return (Assignment n t e)

-- Parser for typ non-terminal
pTyp :: Parser AST
pTyp =
  do
    pToken Col
    t <- pType
    return t
  <|>
  return (Type TyBlank)

-- Parser for stp non-terminal (gives 1.0 as default step amount)
pStp :: Parser AST
pStp =
  do
    pToken Cma
    e <- pExp
    return e
  <|>
  return (Number 1.0)

-- Parser for explist non-terminal
pExplist :: Parser [AST]
pExplist =
  do
    e <- pExp
    (do
       es <- many (do
                     pToken Cma
                     pExplist)
       return (e:(concat es))
     <|>
     return [e])
  <|>
  return []

-- Parser for exp non-terminal
pExp :: Parser AST
pExp =
  do
    n <- pNexp
    e' <- pExp' n
    return e'

-- Parser for exp' non-terminal, given the first operand from exp
pExp' :: AST -> Parser AST
pExp' t =
  do
    pToken (Op Or)
    n <- pNexp
    e' <- pExp' (BinOp BoolOr t n)
    return e'
  <|>
  return t

-- Parser for nexp non-terminal
pNexp :: Parser AST
pNexp =
  do
    c <- pCexp
    n' <- pNexp' c
    return n'

-- Parser for nexp' non-terminal, given the first operand from nexp
pNexp' :: AST -> Parser AST
pNexp' t =
  do
    pToken (Op And)
    c <- pCexp
    n' <- pNexp' (BinOp BoolAnd t c)
    return n'
  <|>
  return t

-- Parser for cexp non-terminal
pCexp :: Parser AST
pCexp =
  do
    a <- pAexp
    c' <- pCexp' a
    return c'

-- Parser for cexp' non-terminal, given the first operand from cexp
pCexp' :: AST -> Parser AST
pCexp' t =
  do
    pToken (Op Less)
    a <- pAexp
    c' <- pCexp' (BinOp LessThan t a)
    return c'
  <|>
  do
    pToken (Op LessEq)
    a <- pAexp
    c' <- pCexp' (BinOp LessEqual t a)
    return c'
  <|>
  do
    pToken (Op Greater)
    a <- pAexp
    c' <- pCexp' (BinOp GreaterThan t a)
    return c'
  <|>
  do
    pToken (Op GreaterEq)
    a <- pAexp
    c' <- pCexp' (BinOp GreaterEqual t a)
    return c'
  <|>
  do
    pToken (Op Eql)
    a <- pAexp
    c' <- pCexp' (BinOp Equal t a)
    return c'
  <|>
  do
    pToken (Op NotEq)
    a <- pAexp
    c' <- pCexp' (BinOp NotEqual t a)
    return c'
  <|>
  return t

-- Parser for aexp non-terminal
pAexp :: Parser AST
pAexp =
  do
    m <- pMexp
    a' <- pAexp' m
    return a'

-- Parser for aexp' non-terminal, given the first operand from aexp
pAexp' :: AST -> Parser AST
pAexp' t =
  do
    pToken (Op Plus)
    m <- pMexp
    a' <- pAexp' (BinOp Addition t m)
    return a'
  <|>
  do
    pToken (Op Minus)
    m <- pMexp
    a' <- pAexp' (BinOp Subtraction t m)
    return a'
  <|>
  return t

-- Parser for mexp non-terminal
pMexp :: Parser AST
pMexp =
  do
    u <- pUn
    m' <- pMexp' u
    return m'

-- Parser for mexp' non-terminal, given the first operand from mexp
pMexp' :: AST -> Parser AST
pMexp' t =
  do
    pToken (Op Times)
    u <- pUn
    m' <- pMexp' (BinOp Multiplication t u)
    return m'
  <|>
  do
    pToken (Op Divide)
    u <- pUn
    m' <- pMexp' (BinOp Division t u)
    return m'
  <|>
  do
    pToken (Op FloorDivide)
    u <- pUn
    m' <- pMexp' (BinOp FloorDivision t u)
    return m'
  <|>
  do
    pToken (Op Modulo)
    u <- pUn
    m' <- pMexp' (BinOp Remainder t u)
    return m'
  <|>
  return t

-- Parser for un non-terminal
pUn :: Parser AST
pUn =
  pEpt
  <|>
  do
    pToken (Op Minus)
    u <- pUn
    return (UnOp Negation u)
  <|>
  do
    pToken (Op Not)
    u <- pUn
    return (UnOp BoolNot u)

-- Parser for ept non-terminal
pEpt :: Parser AST
pEpt =
  do
    f <- pFin
    pToken (Op Power)
    e <- pEpt
    return (BinOp Exponentiation f e)
  <|>
  pFin

-- Parser for fin non-terminal
pFin :: Parser AST
pFin = 
  pNum
  <|>
  do
    n <- pName
    pToken (Par Open)
    es <- pExplist
    pToken (Par Closed)
    return (Call n es)
  <|>
  do
    n <- pName
    pToken (Par SqOpen)
    i <- pExp 
    pToken (Par SqClosed)
    return (ArrayIndex n i)
  <|>
  pName
  <|>
  pStr
  <|>
  pBool
  <|>
  pNil
  <|>
  do
    pToken (Par CrOpen)
    es <- pExplist
    pToken (Par CrClosed)
    return (ArrayInit es)
  <|>
  do
    pToken (Par Open)
    e <- pExp
    pToken (Par Closed)
    return e

-- Parser for num terminal
pNum :: Parser AST
pNum = P(\ts -> case ts of
                  ((Num n):s) -> Just (Number n, s)
                  _           -> Nothing)

-- Parser for name terminal
pName :: Parser AST
pName = P(\ts -> case ts of
                   ((Nme n):s) -> Just (Name n, s)
                   _           -> Nothing)

-- Parser for string terminal
pStr :: Parser AST
pStr = P(\ts -> case ts of
                  ((Str n):s) -> Just (String n, s)
                  _           -> Nothing)

-- Parser for type terminal
pType :: Parser AST
pType = P(\ts -> case ts of
                   ((Tp t):(Par SqOpen):(Num i):(Par SqClosed):s) ->
                      Just (Type (TyArray (TyCon t) (floor i)), s)
                   ((Tp t):s) -> Just (Type (TyCon t), s)
                   _          -> Nothing)

-- Parser for boolean terminal
pBool :: Parser AST
pBool = P(\ts -> case ts of
                   ((Bool b):s) -> Just (Boolean b, s)
                   _            -> Nothing)

-- Parser for nil terminal
pNil :: Parser AST
pNil = P(\ts -> case ts of
                  ((Tp TNil):s) -> Just (Nil, s)
                  _        -> Nothing)