module UnitTests where

import Test.HUnit
import qualified Data.Map as Map

import Utils
import Scanner
import Parser
import TypeInference
import CodeGenerator

-- assignment.lua
assignment :: String
assignment = "x = -1\ny = 2\nz = 3\nx = 1\n\nprint(z * y + x)\n"

-- Tokens for assignment.lua
assignmentToks :: [Token]
assignmentToks = [Nme "x",Eq,Op Minus,Num 1.0,Nme "y",Eq,Num 2.0,Nme "z",Eq,
                  Num 3.0,Nme "x",Eq,Num 1.0,Kword Prt,Par Open,Nme "z",
                  Op Times,Nme "y",Op Plus,Nme "x",Par Closed]

-- AST for assignment.lua
assignmentAST :: AST
assignmentAST = Statements [Assignment (Name "x") (Type TyBlank) (UnOp Negation
                (Number 1.0)),Assignment (Name "y") (Type TyBlank) (Number 2.0),
                Assignment (Name "z") (Type TyBlank) (Number 3.0),Assignment
                (Name "x") (Type TyBlank) (Number 1.0),Print (BinOp Addition
                (BinOp Multiplication (Name "z") (Name "y")) (Name "x"))]

-- TypeEnvt for assignment.lua
assignmentTypeEnvt :: TypeEnvt
assignmentTypeEnvt = Map.fromList [("x",TyCon TNumber), ("y",TyCon TNumber),
                                   ("z",TyCon TNumber)]

-- LLVM IR code for assignment.lua
assignmentCode :: ([String], [String], [String])
assignmentCode = (["@x = global double 0.0","@y = global double 0.0","@z = global double 0.0"],
                  [],
                  ["%r0 = fsub double 0.0, 1.0","store double %r0, double* @x",
                   "store double 2.0, double* @y","store double 3.0, double* @z",
                   "store double 1.0, double* @x","%r1 = load double, double* @z",
                   "%r2 = load double, double* @y","%r3 = fmul double %r1, %r2",
                   "%r4 = load double, double* @x","%r5 = fadd double %r3, %r4",
                   "call i32 (i8*,...) @printf(i8* @-formatDouble, double %r5)"])

-- conditionals.lua
conditionals :: String
conditionals = "x = 2\ny = true\n\nif x == 0 then\n    if y then\n" ++
               "        print(\"a\")\n    end\nelseif x == 1 then\n" ++
               "    if y then\n        print(\"b\")\n    end\nelseif" ++
               " x == 2 then\n    if y then\n        print(\"c\")\n " ++
               "   end\nelse\n    if y then\n        print(\"d\")\n    end\nend"

-- Tokens for conditionals.lua
conditionalsToks :: [Token]
conditionalsToks = [Nme "x",Eq,Num 2.0,Nme "y",Eq,Bool True,Kword If,Nme "x",
                    Op Eql,Num 0.0,Kword Then,Kword If,Nme "y",Kword Then,
                    Kword Prt,Par Open,Str "a",Par Closed,Kword End,Kword ElseIf,
                    Nme "x",Op Eql,Num 1.0,Kword Then,Kword If,Nme "y",Kword Then,
                    Kword Prt,Par Open,Str "b",Par Closed,Kword End,Kword ElseIf,
                    Nme "x",Op Eql,Num 2.0,Kword Then,Kword If,Nme "y",Kword Then,
                    Kword Prt,Par Open,Str "c",Par Closed,Kword End,Kword Else,
                    Kword If,Nme "y",Kword Then,Kword Prt,Par Open,Str "d",
                    Par Closed,Kword End,Kword End]

-- AST for conditionals.lua
conditionalsAST :: AST
conditionalsAST = Statements [Assignment (Name "x") (Type TyBlank) (Number 2.0),
                  Assignment (Name "y") (Type TyBlank) (Boolean True),IfElse
                  (BinOp Equal (Name "x") (Number 0.0)) (Statements [IfThen
                  (Name "y") (Statements [Print (String "a")])]) (IfElse
                  (BinOp Equal (Name "x") (Number 1.0)) (Statements [IfThen
                  (Name "y") (Statements [Print (String "b")])]) (IfElse
                  (BinOp Equal (Name "x") (Number 2.0)) (Statements [IfThen
                  (Name "y") (Statements [Print (String "c")])]) (Statements
                  [IfThen (Name "y") (Statements [Print (String "d")])])))]

-- TypeEnvt for conditionals.lua
conditionalsTypeEnvt :: TypeEnvt
conditionalsTypeEnvt = Map.fromList [("x",TyCon TNumber), ("y",TyCon TBoolean)]

-- LLVM IR code for conditionals.lua
conditionalsCode :: ([String], [String], [String])
conditionalsCode = (["@x = global double 0.0","@y = global i1 0"],
                    ["@\"-d\" = constant [2 x i8] c\"d\\00\"",
                     "@\"-c\" = constant [2 x i8] c\"c\\00\"",
                     "@\"-b\" = constant [2 x i8] c\"b\\00\"",
                     "@\"-a\" = constant [2 x i8] c\"a\\00\""],
                    ["store double 2.0, double* @x","store i1 1, i1* @y",
                     "%r0 = load double, double* @x","%r1 = fcmp oeq double %r0, 0.0",
                     "br i1 %r1, label %-15, label %-16","-15:","%r2 = load i1, i1* @y",
                     "br i1 %r2, label %-1, label %-2","-1:",
                     "call i32 (i8*,...) @printf(i8* @-formatString, i8* @\"-a\")",
                     "br label %-2","-2:","br label %-17","-16:",
                     "%r3 = load double, double* @x","%r4 = fcmp oeq double %r3, 1.0",
                     "br i1 %r4, label %-12, label %-13","-12:","%r5 = load i1, i1* @y",
                     "br i1 %r5, label %-3, label %-4","-3:",
                     "call i32 (i8*,...) @printf(i8* @-formatString, i8* @\"-b\")","br label %-4",
                     "-4:","br label %-14","-13:","%r6 = load double, double* @x",
                     "%r7 = fcmp oeq double %r6, 2.0","br i1 %r7, label %-9, label %-10","-9:",
                     "%r8 = load i1, i1* @y","br i1 %r8, label %-5, label %-6","-5:",
                     "call i32 (i8*,...) @printf(i8* @-formatString, i8* @\"-c\")","br label %-6",
                     "-6:","br label %-11","-10:","%r9 = load i1, i1* @y","br i1 %r9, label %-7, label %-8",
                     "-7:","call i32 (i8*,...) @printf(i8* @-formatString, i8* @\"-d\")","br label %-8","-8:",
                     "br label %-11","-11:","br label %-14","-14:","br label %-17","-17:"])

-- loops.lua
loops :: String
loops = "x = 4\ny = 8\n\nfor i = 0, x do\n    j = 0\n    while j <= y do\n" ++
        "        print(i + j)\n\n        if j == 7 then\n            break\n" ++
        "        else\n            j = j + 1\n        end\n    end\nend\n\n" ++
        "z = 0\nrepeat\n    z = z + 1\nuntil z == 10"

-- Tokens for loops.lua
loopsToks :: [Token]
loopsToks = [Nme "x",Eq,Num 4.0,Nme "y",Eq,Num 8.0,Kword For,Nme "i",Eq,Num 0.0,
             Cma,Nme "x",Kword Do,Nme "j",Eq,Num 0.0,Kword While,Nme "j",
             Op LessEq,Nme "y",Kword Do,Kword Prt,Par Open,Nme "i",Op Plus,
             Nme "j",Par Closed,Kword If,Nme "j",Op Eql,Num 7.0,Kword Then,
             Kword Brk,Kword Else,Nme "j",Eq,Nme "j",Op Plus,Num 1.0,Kword End,
             Kword End,Kword End,Nme "z",Eq,Num 0.0,Kword Rpt,Nme "z",Eq,Nme "z",
             Op Plus,Num 1.0,Kword Utl,Nme "z",Op Eql,Num 10.0]

-- AST for loops.lua
loopsAST :: AST
loopsAST = Statements [Assignment (Name "x") (Type TyBlank) (Number 4.0),
           Assignment (Name "y") (Type TyBlank) (Number 8.0),ForLoop
           (Assignment (Name "i") (Type TyBlank) (Number 0.0)) (Name "x") (Number 1.0) 
           (Statements [Assignment (Name "j") (Type TyBlank) (Number 0.0),WhileLoop
           (BinOp LessEqual (Name "j") (Name "y")) (Statements [Print (BinOp Addition
           (Name "i") (Name "j")),IfElse (BinOp Equal (Name "j") (Number 7.0))
           (Statements [Break]) (Statements [Assignment (Name "j") (Type TyBlank)
           (BinOp Addition (Name "j") (Number 1.0))])])]),Assignment (Name "z")
           (Type TyBlank) (Number 0.0),RepeatUntil (Statements [Assignment (Name "z")
           (Type TyBlank) (BinOp Addition (Name "z") (Number 1.0))]) (BinOp Equal
           (Name "z") (Number 10.0))]

-- TypeEnvt for loops.lua
loopsTypeEnvt :: TypeEnvt
loopsTypeEnvt = Map.fromList [("i", TyCon TNumber), ("j", TyCon TNumber),
                              ("x",TyCon TNumber), ("y", TyCon TNumber),
                              ("z",TyCon TNumber)]

-- LLVM IR code for loops.lua
loopsCode :: ([String], [String], [String])
loopsCode = (["@i = global double 0.0","@j = global double 0.0","@x = global double 0.0",
              "@y = global double 0.0","@z = global double 0.0"],
             [],
             ["store double 4.0, double* @x","store double 8.0, double* @y","%r0 = alloca double",
             "store double 0.0, double* @i","%r1 = fcmp olt double 1.0, 0.0","br i1 %r1, label %-1, label %-2",
             "-1:","store double -1.0, double* %r0","br label %-3","-2:","store double 1.0, double* %r0",
             "br label %-3","-3:","br label %-10","-10:","%r2 = load double, double* %r0",
             "%r3 = load double, double* @i","%r4 = fmul double %r2, %r3","%r5 = load double, double* %r0",
             "%r6 = load double, double* @x","%r7 = fmul double %r5, %r6","%r8 = fcmp ole double %r4, %r7",
             "br i1 %r8, label %-11, label %-12","-11:","store double 0.0, double* @j","br label %-7","-7:",
             "%r11 = load double, double* @j","%r12 = load double, double* @y",
             "%r13 = fcmp ole double %r11, %r12","br i1 %r13, label %-8, label %-9","-8:",
             "%r14 = load double, double* @i","%r15 = load double, double* @j","%r16 = fadd double %r14, %r15",
             "call i32 (i8*,...) @printf(i8* @-formatDouble, double %r16)","%r17 = load double, double* @j",
             "%r18 = fcmp oeq double %r17, 7.0","br i1 %r18, label %-4, label %-5","-4:","br label %-9","-5:",
             "%r19 = load double, double* @j","%r20 = fadd double %r19, 1.0","store double %r20, double* @j",
             "br label %-6","-6:","br label %-7","-9:","%r9 = load double, double* @i","%r10 = fadd double %r9, 1.0",
             "store double %r10, double* @i","br label %-10","-12:","store double 0.0, double* @z","br label %-13","-13:",
             "%r21 = load double, double* @z","%r22 = fadd double %r21, 1.0","store double %r22, double* @z",
             "%r23 = load double, double* @z","%r24 = fcmp oeq double %r23, 10.0","br i1 %r24, label %-14, label %-13","-14:"])

-- functions.lua
functions :: String
functions = "function fizzbuzz(n)\n    if n % 3 == 0 and n % 5 == 0 then\n" ++
            "        print(\"fizzbuzz\")\n    elseif n % 3 == 0 then\n    " ++
            "    print(\"fizz\")\n    elseif n % 5 == 0 then\n        " ++
            "print(\"buzz\")\n    else\n        print(n)\n    end\nend\n\n" ++
            "for i = 1, 30 do\n    fizzbuzz(i)\nend"

-- Tokens for functions.lua
functionsToks :: [Token]
functionsToks = [Kword Func,Nme "fizzbuzz",Par Open,Nme "n",Par Closed,Kword If,
                 Nme "n",Op Modulo,Num 3.0,Op Eql,Num 0.0,Op And,Nme "n",Op Modulo,
                 Num 5.0,Op Eql,Num 0.0,Kword Then,Kword Prt,Par Open,
                 Str "fizzbuzz",Par Closed,Kword ElseIf,Nme "n",Op Modulo,
                 Num 3.0,Op Eql,Num 0.0,Kword Then,Kword Prt,Par Open,Str "fizz",
                 Par Closed,Kword ElseIf,Nme "n",Op Modulo,Num 5.0,Op Eql,
                 Num 0.0,Kword Then,Kword Prt,Par Open,Str "buzz",Par Closed,
                 Kword Else,Kword Prt,Par Open,Nme "n",Par Closed,Kword End,
                 Kword End,Kword For,Nme "i",Eq,Num 1.0,Cma,Num 30.0,Kword Do,
                 Nme "fizzbuzz",Par Open,Nme "i",Par Closed,Kword End]

-- AST for functions.lua
functionsAST :: AST
functionsAST = Statements [Function (Name "fizzbuzz") [Arg (Name "-fizzbuzz-n")
               (Type TyBlank)] (Statements [IfElse (BinOp BoolAnd (BinOp Equal
               (BinOp Remainder (Name "-fizzbuzz-n") (Number 3.0)) (Number 0.0))
               (BinOp Equal (BinOp Remainder (Name "-fizzbuzz-n") (Number 5.0))
               (Number 0.0))) (Statements [Print (String "fizzbuzz")]) (IfElse
               (BinOp Equal (BinOp Remainder (Name "-fizzbuzz-n") (Number 3.0))
               (Number 0.0)) (Statements [Print (String "fizz")]) (IfElse
               (BinOp Equal (BinOp Remainder (Name "-fizzbuzz-n") (Number 5.0))
               (Number 0.0)) (Statements [Print (String "buzz")]) (Statements
               [Print (Name "-fizzbuzz-n")])))]),ForLoop (Assignment (Name "i")
               (Type TyBlank) (Number 1.0)) (Number 30.0) (Number 1.0)
               (Statements [Call (Name "fizzbuzz") [Name "i"]])]

-- TypeEnvt for functions.lua
functionsTypeEnvt :: TypeEnvt
functionsTypeEnvt = Map.fromList [("-fizzbuzz-n",TyCon TNumber),
                                  ("-r-fizzbuzz-n",TyCon TNumber),
                                  ("fizzbuzz", TyArr [TyCon TNumber] (TyCon TNil)),
                                  ("i",TyCon TNumber)]

-- LLVM IR code for functions.lua
functionsCode :: ([String], [String], [String])
functionsCode = (["@i = global double 0.0"],
                 ["@\"-buzz\" = constant [5 x i8] c\"buzz\\00\"","@\"-fizz\" = constant [5 x i8] c\"fizz\\00\"",
                  "@\"-fizzbuzz\" = constant [9 x i8] c\"fizzbuzz\\00\"","define i1* @fizzbuzz(double %-fizzbuzz-n) {",
                  "%-r-fizzbuzz-n = alloca double","store double %-fizzbuzz-n, double* %-r-fizzbuzz-n",
                  "%r0 = load double, double* %-r-fizzbuzz-n","%r1 = frem double %r0, 3.0","%r2 = fcmp oeq double %r1, 0.0",
                  "%r3 = load double, double* %-r-fizzbuzz-n","%r4 = frem double %r3, 5.0","%r5 = fcmp oeq double %r4, 0.0",
                  "%r6 = and i1 %r2, %r5","br i1 %r6, label %-7, label %-8","-7:",
                  "call i32 (i8*,...) @printf(i8* @-formatString, i8* @\"-fizzbuzz\")","br label %-9","-8:",
                  "%r7 = load double, double* %-r-fizzbuzz-n","%r8 = frem double %r7, 3.0","%r9 = fcmp oeq double %r8, 0.0",
                  "br i1 %r9, label %-4, label %-5","-4:","call i32 (i8*,...) @printf(i8* @-formatString, i8* @\"-fizz\")",
                  "br label %-6","-5:","%r10 = load double, double* %-r-fizzbuzz-n","%r11 = frem double %r10, 5.0",
                  "%r12 = fcmp oeq double %r11, 0.0","br i1 %r12, label %-1, label %-2","-1:",
                  "call i32 (i8*,...) @printf(i8* @-formatString, i8* @\"-buzz\")","br label %-3","-2:",
                  "%r13 = load double, double* %-r-fizzbuzz-n","call i32 (i8*,...) @printf(i8* @-formatDouble, double %r13)",
                  "br label %-3","-3:","br label %-6","-6:","br label %-9","-9:","ret i1* null","}"],
                 ["%r14 = alloca double", "store double 1.0, double* @i","%r15 = fcmp olt double 1.0, 0.0",
                 "br i1 %r15, label %-10, label %-11","-10:","store double -1.0, double* %r14","br label %-12","-11:",
                 "store double 1.0, double* %r14","br label %-12","-12:","br label %-13","-13:","%r16 = load double, double* %r14",
                 "%r17 = load double, double* @i","%r18 = fmul double %r16, %r17","%r19 = load double, double* %r14",
                 "%r20 = fmul double %r19, 30.0","%r21 = fcmp ole double %r18, %r20","br i1 %r21, label %-14, label %-15","-14:",
                 "%r24 = load double, double* @i","%r25 = call i1* @fizzbuzz(double %r24)","%r22 = load double, double* @i",
                 "%r23 = fadd double %r22, 1.0","store double %r23, double* @i","br label %-13","-15:"])

-- arrays.lua
arrays :: String
arrays = "x = {1, 2, 3, 4, 5, 6}\n\nfor i = 1, 6 do\n    x[i] = x[i] * x[i]\n" ++
         "end\n\nfor i = 1, 6 do\n    print(x[i])\nend\n\nx = {}\nprint(x[6])"

-- Tokens for arrays.lua
arraysToks :: [Token]
arraysToks = [Nme "x",Eq,Par CrOpen,Num 1.0,Cma,Num 2.0,Cma,Num 3.0,Cma,Num 4.0,
              Cma,Num 5.0,Cma,Num 6.0,Par CrClosed,Kword For,Nme "i",Eq,Num 1.0,
              Cma,Num 6.0,Kword Do,Nme "x",Par SqOpen,Nme "i",Par SqClosed,Eq,
              Nme "x",Par SqOpen,Nme "i",Par SqClosed,Op Times,Nme "x",Par SqOpen,
              Nme "i",Par SqClosed,Kword End,Kword For,Nme "i",Eq,Num 1.0,Cma,
              Num 6.0,Kword Do,Kword Prt,Par Open,Nme "x",Par SqOpen,Nme "i",
              Par SqClosed,Par Closed,Kword End,Nme "x",Eq,Par CrOpen,Par CrClosed,
              Kword Prt,Par Open,Nme "x",Par SqOpen,Num 6.0,Par SqClosed,Par Closed]

-- AST for arrays.lua
arraysAST :: AST
arraysAST = Statements [Assignment (Name "x") (Type TyBlank) (ArrayInit
            [Number 1.0,Number 2.0,Number 3.0,Number 4.0,Number 5.0,Number 6.0]),
            ForLoop (Assignment (Name "i") (Type TyBlank) (Number 1.0)) (Number 6.0)
            (Number 1.0) (Statements [Assignment (ArrayIndex (Name "x") (Name "i"))
            (Type TyBlank) (BinOp Multiplication (ArrayIndex (Name "x") (Name "i"))
            (ArrayIndex (Name "x") (Name "i")))]),ForLoop (Assignment (Name "i")
            (Type TyBlank) (Number 1.0)) (Number 6.0) (Number 1.0) (Statements
            [Print (ArrayIndex (Name "x") (Name "i"))]),Assignment (Name "x")
            (Type TyBlank) (ArrayInit []),Print (ArrayIndex (Name "x") (Number 6.0))]

-- TypeEnvt for arrays.lua
arraysTypeEnvt :: TypeEnvt
arraysTypeEnvt = Map.fromList [("i",TyCon TNumber), ("x",TyArray (TyCon TNumber) 6)]

-- LLVM IR code for arrays.lua
arraysCode :: ([String], [String], [String])
arraysCode = (["@i = global double 0.0","@x = global [6 x double] zeroinitializer"],
              [],
              ["store [6 x double] [double 1.0, double 2.0, double 3.0, double 4.0, double 5.0, double 6.0], [6 x double]* @x",
               "%r0 = alloca double","store double 1.0, double* @i","%r1 = fcmp olt double 1.0, 0.0",
               "br i1 %r1, label %-1, label %-2","-1:","store double -1.0, double* %r0","br label %-3","-2:",
               "store double 1.0, double* %r0","br label %-3","-3:","br label %-4","-4:","%r2 = load double, double* %r0",
               "%r3 = load double, double* @i","%r4 = fmul double %r2, %r3","%r5 = load double, double* %r0",
               "%r6 = fmul double %r5, 6.0","%r7 = fcmp ole double %r4, %r6","br i1 %r7, label %-5, label %-6","-5:",
               "%r10 = load double, double* @i","%r11 = fsub double %r10, 1.0","%r12 = load double, double* @i",
               "%r13 = fsub double %r12, 1.0","%r14 = fptoui double %r13 to i32","%r15 = getelementptr double, double* @x, i32 %r14",
               "%r16 = load double, double* %r15","%r17 = load double, double* @i","%r18 = fsub double %r17, 1.0",
               "%r19 = fptoui double %r18 to i32","%r20 = getelementptr double, double* @x, i32 %r19","%r21 = load double, double* %r20",
               "%r22 = fmul double %r16, %r21","%r24 = fptoui double %r11 to i32","%r23 = getelementptr double, double* @x, i32 %r24",
               "store double %r22, double* %r23","%r8 = load double, double* @i","%r9 = fadd double %r8, 1.0","store double %r9, double* @i",
               "br label %-4","-6:","%r25 = alloca double","store double 1.0, double* @i","%r26 = fcmp olt double 1.0, 0.0",
               "br i1 %r26, label %-7, label %-8","-7:","store double -1.0, double* %r25","br label %-9","-8:","store double 1.0, double* %r25",
               "br label %-9","-9:","br label %-10","-10:","%r27 = load double, double* %r25","%r28 = load double, double* @i",
               "%r29 = fmul double %r27, %r28","%r30 = load double, double* %r25","%r31 = fmul double %r30, 6.0","%r32 = fcmp ole double %r29, %r31",
               "br i1 %r32, label %-11, label %-12","-11:","%r35 = load double, double* @i","%r36 = fsub double %r35, 1.0",
               "%r37 = fptoui double %r36 to i32","%r38 = getelementptr double, double* @x, i32 %r37","%r39 = load double, double* %r38",
               "call i32 (i8*,...) @printf(i8* @-formatDouble, double %r39)","%r33 = load double, double* @i","%r34 = fadd double %r33, 1.0",
               "store double %r34, double* @i","br label %-10","-12:","store [6 x double] zeroinitializer, [6 x double]* @x","%r40 = fsub double 6.0, 1.0",
               "%r41 = fptoui double %r40 to i32","%r42 = getelementptr double, double* @x, i32 %r41","%r43 = load double, double* %r42",
               "call i32 (i8*,...) @printf(i8* @-formatDouble, double %r43)"])

-- types.lua
types :: String
types = "x : number = 1\ny : boolean = true\nz : nil = nil\ns : string = " ++
        "\"string\"\n\nfunction types(n : number)\n    a : number[1] = {0}\n" ++
        "    for i : number = 1, x do\n        a[i] : number = n\n    " ++
        "end\nend\n\ntypes(8)"

-- Tokens for types.lua
typesToks :: [Token]
typesToks = [Nme "x",Col,Tp TNumber,Eq,Num 1.0,Nme "y",Col,Tp TBoolean,Eq,Bool True,
             Nme "z",Col,Tp TNil,Eq,Tp TNil,Nme "s",Col,Tp TString,Eq,Str "string",
             Kword Func,Nme "types",Par Open,Nme "n",Col,Tp TNumber,Par Closed,
             Nme "a",Col,Tp TNumber,Par SqOpen,Num 1.0,Par SqClosed,Eq,Par CrOpen,
             Num 0.0,Par CrClosed,Kword For,Nme "i",Col,Tp TNumber,Eq,Num 1.0,Cma,
             Nme "x",Kword Do,Nme "a",Par SqOpen,Nme "i",Par SqClosed,Col,Tp TNumber,
             Eq,Nme "n",Kword End,Kword End,Nme "types",Par Open,Num 8.0,Par Closed]

-- AST for types.lua
typesAST :: AST
typesAST = Statements [Assignment (Name "x") (Type (TyCon TNumber)) (Number 1.0),Assignment
           (Name "y") (Type (TyCon TBoolean)) (Boolean True),Assignment (Name "z") (Type (TyCon TNil)) Nil,
           Assignment (Name "s") (Type (TyCon TString)) (String "string"),Function (Name "types")
           [Arg (Name "-types-n") (Type (TyCon TNumber))] (Statements [Assignment (Name "a")
           (Type (TyArray (TyCon TNumber) 1)) (ArrayInit [Number 0.0]),ForLoop (Assignment (Name "i")
           (Type (TyCon TNumber)) (Number 1.0)) (Name "x") (Number 1.0) (Statements [Assignment
           (ArrayIndex (Name "a") (Name "i")) (Type (TyCon TNumber)) (Name "-types-n")])]),
           Call (Name "types") [Number 8.0]]

-- TypeEnvt for types.lua
typesTypeEnvt :: TypeEnvt
typesTypeEnvt = Map.fromList [("-r-types-n",TyCon TNumber), ("-types-n",TyCon TNumber),
                              ("a",TyArray (TyCon TNumber) 1), ("i",TyCon TNumber),
                              ("s",TyCon TString), ("types",TyArr [TyCon TNumber] (TyCon TNil)),
                              ("x",TyCon TNumber), ("y",TyCon TBoolean), ("z",TyCon TNil)]

-- LLVM IR code for types.lua
typesCode :: ([String], [String], [String])
typesCode = (["@a = global [1 x double] zeroinitializer","@i = global double 0.0","@s = global i8* null",
              "@x = global double 0.0","@y = global i1 0","@z = global i1* null"],
             ["@\"-string\" = constant [7 x i8] c\"string\\00\"","define i1* @types(double %-types-n) {","%-r-types-n = alloca double",
              "store double %-types-n, double* %-r-types-n","store [1 x double] [double 0.0], [1 x double]* @a","%r1 = alloca double",
              "store double 1.0, double* @i","%r2 = fcmp olt double 1.0, 0.0","br i1 %r2, label %-1, label %-2","-1:",
              "store double -1.0, double* %r1","br label %-3","-2:","store double 1.0, double* %r1","br label %-3","-3:","br label %-4",
              "-4:","%r3 = load double, double* %r1","%r4 = load double, double* @i","%r5 = fmul double %r3, %r4",
              "%r6 = load double, double* %r1","%r7 = load double, double* @x","%r8 = fmul double %r6, %r7","%r9 = fcmp ole double %r5, %r8",
              "br i1 %r9, label %-5, label %-6","-5:","%r12 = load double, double* @i","%r13 = fsub double %r12, 1.0",
              "%r14 = load double, double* %-r-types-n","%r16 = fptoui double %r13 to i32","%r15 = getelementptr double, double* @a, i32 %r16",
              "store double %r14, double* %r15","%r10 = load double, double* @i","%r11 = fadd double %r10, 1.0","store double %r11, double* @i",
              "br label %-4","-6:","ret i1* null","}"],
             ["store double 1.0, double* @x","store i1 1, i1* @y","store i1* null, i1** @z","%r0 = getelementptr i8*, i8** @\"-string\", i32 0",
              "store i8* %r0, i8** @s","%r17 = call i1* @types(double 8.0)"])

-- Test for the scanner
scannerTest :: Test
scannerTest = TestCase
  (do
    assertEqual "Scanning assignment" (scanSrc assignment) assignmentToks
    assertEqual "Scanning conditionals" (scanSrc conditionals) conditionalsToks
    assertEqual "Scanning loops" (scanSrc loops) loopsToks
    assertEqual "Scanning functions" (scanSrc functions) functionsToks
    assertEqual "Scanning arrays" (scanSrc arrays) arraysToks
    assertEqual "Scanning types" (scanSrc types) typesToks)

-- Test for the parser
parserTest :: Test
parserTest = TestCase
  (do
    assertEqual "Parsing assignment" (parseTokens assignmentToks) assignmentAST
    assertEqual "Parsing conditionals" (parseTokens conditionalsToks) conditionalsAST
    assertEqual "Parsing loops" (parseTokens loopsToks) loopsAST
    assertEqual "Parsing functions" (parseTokens functionsToks) functionsAST
    assertEqual "Parsing arrays" (parseTokens arraysToks) arraysAST
    assertEqual "Parsing types" (parseTokens typesToks) typesAST)

-- Test for type inference
typeInferenceTest :: Test
typeInferenceTest = TestCase
  (do
    assertEqual "Type inference for assignment" (inferTypes assignmentAST) assignmentTypeEnvt
    assertEqual "Type inference for conditionals" (inferTypes conditionalsAST) conditionalsTypeEnvt
    assertEqual "Type inference for loops" (inferTypes loopsAST) loopsTypeEnvt
    assertEqual "Type inference for functions" (inferTypes functionsAST) functionsTypeEnvt
    assertEqual "Type inference for arrays" (inferTypes arraysAST) arraysTypeEnvt
    assertEqual "Type inference for types" (inferTypes typesAST) typesTypeEnvt)

-- Test for the code generator
codeGeneratorTest :: Test
codeGeneratorTest = TestCase
  (do
    assertEqual "Code generation for assignment" (generateCode assignmentAST assignmentTypeEnvt) assignmentCode
    assertEqual "Code generation for conditionals" (generateCode conditionalsAST conditionalsTypeEnvt) conditionalsCode
    assertEqual "Code generation for loops" (generateCode loopsAST loopsTypeEnvt) loopsCode
    assertEqual "Code generation for functions" (generateCode functionsAST functionsTypeEnvt) functionsCode
    assertEqual "Code generation for arrays" (generateCode arraysAST arraysTypeEnvt) arraysCode
    assertEqual "Code generation for types" (generateCode typesAST typesTypeEnvt) typesCode)

-- Run the tests
runTests :: IO Counts 
runTests = 
  do
    runTestTT scannerTest
    runTestTT parserTest
    runTestTT typeInferenceTest
    runTestTT codeGeneratorTest