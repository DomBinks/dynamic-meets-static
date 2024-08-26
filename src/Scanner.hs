module Scanner where

import Control.Applicative
import Data.Char

import Utils

-- Scan a program String and return the corresponding list of Tokens
scanSrc :: String -> [Token]
scanSrc s = case (scan scanner s) of
              (Just (ts, l)) -> if l == "" then ts else error "Invalid syntax"
              Nothing -> error "Unable to scan program"
  
-- Scan a program String
scanner :: Scanner [Token]
scanner =
  do
    sSpace
    ts <- many (
            sOp <|>
            sPar
            <|>
            sSCol
            <|>
            sCol
            <|>
            sEql
            <|>
            sCma
            <|>
            sKword
            <|>
            sTp
            <|>
            sNum
            <|>
            sBool
            <|>
            sNme
            <|>
            sStr)
    sSpace
    return ts

-- Scan an operator
sOp :: Scanner Token
sOp =
  do
    sSpace
    op <- sStrings (map fst operatorMap)
    let tok = case (lookup op operatorMap) of
                (Just t) -> t
                Nothing -> error ("Unable to get token for operator " ++ op)
    return tok

-- Scan a paranthesis
sPar :: Scanner Token
sPar = 
  do
    sSpace
    sChar '('
    return (Par Open)
  <|>
  do
    sSpace
    sChar ')'
    return (Par Closed)
  <|>
  do
    sSpace
    sChar '['
    return (Par SqOpen)
  <|>
  do
    sSpace
    sChar ']'
    return (Par SqClosed)
  <|>
  do
    sSpace
    sChar '{'
    return (Par CrOpen)
  <|>
  do
    sSpace
    sChar '}'
    return (Par CrClosed)

-- Scan a semicolon
sSCol :: Scanner Token
sSCol =
  do
    sSpace
    sChar ';'
    return SCol

-- Scan a colon
sCol :: Scanner Token
sCol =
  do
    sSpace
    sChar ':'
    return Col

-- Scan an equals
sEql :: Scanner Token
sEql =
  do
    sSpace
    sChar '='
    return Eq

-- Scan a comma
sCma :: Scanner Token
sCma =
  do
    sSpace
    sChar ','
    return Cma

-- Scan a keyword
sKword :: Scanner Token
sKword =
  do
    sSpace
    kw <- sStrings (map fst keywordMap)
    let tok = case (lookup kw keywordMap) of
                (Just t) -> t
                Nothing -> error ("Unable to get token for keyword " ++ kw)
    return tok

-- Scan a name
sNme :: Scanner Token
sNme =
  do
    sSpace
    -- first character can be a character or underscore
    c <- (sSatisfy isAlpha <|> (sChar '_')) 
    -- subsequent characters can be character, number, or underscore
    cs <- many ((sSatisfy isAlphaNum) <|> (sChar '_'))
    if elem (c:cs) reservedList -- check the name isn't in the reserved list
    then error ((c:cs) ++ " can't be used as the name of a variable or function")
    else return (Nme (c:cs))

-- Scanner for scanning a string
sStr :: Scanner Token
sStr =
  do
    sSpace
    sChar '\"'
    cs <- some (sSatisfy $ not . ((==) '\"'))
    sChar '\"'
    return (Str cs)

-- Scan a number
sNum :: Scanner Token
sNum =
  do
    sSpace
    ds <- sDigits
    (do
      fs <- sDecimal
      (do
          es <- sExp
          return (Num (read (ds ++ fs ++ es) :: Double))
        <|>
        return (Num (read (ds ++ fs) :: Double)))
     <|>
     do
       es <- sExp
       return (Num (read (ds ++ es) :: Double))
     <|>
     return (Num (read ds :: Double)))

-- Scan a sequence of digits
sDigits :: Scanner String
sDigits =
  do
    ds <- some (sSatisfy (isDigit))
    return ds

-- Scan the decimal part of a number
sDecimal :: Scanner String
sDecimal =
  do
    d <- sChar '.'
    ds <- sDigits
    return (d:ds)

-- Scan the exponent part of a number
sExp :: Scanner String
sExp =
  do
    e <- sChar 'e'
    (do
      m <- sChar '-'
      ds <- sDigits
      return (e:m:ds)
     <|>
     do
       ds <- sDigits
       return (e:ds))

-- Scan a type
sTp :: Scanner Token
sTp =
  do
    sSpace
    t <- sString "nil"
    return (Tp TNil)
  <|>
  do
    sSpace
    t <- sString "boolean"
    return (Tp TBoolean)
  <|>
  do
    sSpace
    t <- sString "number"
    return (Tp TNumber)
  <|>
  do
    sSpace
    t <- sString "string"
    return (Tp TString)

-- Scan a boolean
sBool :: Scanner Token
sBool =
  do
    sSpace
    b <- sString "true"
    return (Bool True)
  <|>
  do
    sSpace
    b <- sString "false"
    return (Bool False)

-- Take a list of Strings and return a combinator for scanning them
sStrings :: [String] -> Scanner String
sStrings (x:[]) = sString x
sStrings (x:xs) = sString x <|> sStrings xs

-- Map Strings to their corresponding operator Token
operatorMap :: [(String, Token)]
operatorMap =
  [ ("+", Op Plus)
  , ("-", Op Minus)
  , ("*", Op Times)
  , ("//", Op FloorDivide)
  , ("/", Op Divide)
  , ("%", Op Modulo)
  , ("^", Op Power)
  , ("<=", Op LessEq)
  , ("<", Op Less)
  , (">=", Op GreaterEq)
  , (">", Op Greater)
  , ("==", Op Eql)
  , ("~=", Op NotEq)
  , ("not", Op Not)
  , ("and", Op And)
  , ("or", Op Or)
  ]

-- Map Strings to their corresponding keyword Token
keywordMap :: [(String, Token)]
keywordMap = 
  [ ("if", Kword If)
  , ("then", Kword Then)
  , ("elseif", Kword ElseIf)
  , ("else", Kword Else)
  , ("end", Kword End)
  , ("while", Kword While)
  , ("do", Kword Do)
  , ("for", Kword For)
  , ("function", Kword Func)
  , ("return", Kword Ret)
  , ("break", Kword Brk)
  , ("print", Kword Prt)
  , ("repeat", Kword Rpt)
  , ("until", Kword Utl)
  ]