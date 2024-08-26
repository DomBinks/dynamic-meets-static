# Dynamic Meets Static: Ahead-of-time Compilation for Lua

- To compile the compiler, simply compile Main.hs using GHC.
- To compile a Lua file, simply provide the Lua file to the compiler as a command line argument, remembering to include the file extension.
    - This produces a .ll file of the LLVM IR produced and a .out file for the executable
- In order for the compiler to operate correctly, the template file must be in the same directory, alongside the Lua program being compiled.