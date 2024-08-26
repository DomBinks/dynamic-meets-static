module Main where

import System.Environment
import System.Process

import Utils
import Scanner
import Parser
import TypeInference
import CodeGenerator

-- Extract the file name from a file
fileName :: String -> String
fileName "" = ""
fileName ('.':_) = ""
fileName (x:xs) = x : fileName xs

-- Read in the template file and return a list of lines
readTemplate :: String -> IO [String]
readTemplate file = do
                      code <- readFile file
                      return (lines code)

-- Given a list of lines of code from the template file, a list of lines of the
-- code to insert, and the line number to insert the code at, return a list of
-- lines of code with the code inserted at that line
insertInCode :: [String] -> [String] -> Int -> [String]
insertInCode code func 0 = func ++ code
insertInCode (c:ode) func x = c : insertInCode ode func (x-1)

-- Write to the list of lines to the file
writeProgram :: String -> [String] -> IO ()
writeProgram file program = do
                              let content = unlines program
                              writeFile file content

-- Run the compiler
main :: IO ()
main = do
         args <- getArgs      -- Get the command line arguments
         let file = head args -- Get the first argument i.e. the file to compile
         src <- readFile file -- Read in the file to compile
         let
           tokens = scanSrc src     -- Scan the file into a list of tokens
           ast = parseTokens tokens -- Parse the list of tokens into an AST
           typeEnv = inferTypes ast -- Type check the AST and get the type environment
           (vars, strFunc, code) = generateCode ast typeEnv -- Generate the code for the AST using the type environment
         template <- readTemplate "template.ll" -- Read in the template file
         let
           program = insertInCode template vars 44 -- Insert variables into the template
           program' = insertInCode program strFunc (47 + (length vars)) -- Insert strings and functions into the template
           program'' = insertInCode program' code (52 + (length vars) + (length strFunc)) -- Insert main code into the template
           name = fileName file -- Get the file name of the file
         writeProgram (name ++ ".ll") program'' -- Write the compiled file
         callCommand ("clang -lm " ++ (name ++ ".ll") ++ " -o " ++ (name ++ ".out")) -- Compile the compiled file with Clang