# What is unwind-mc

unwind-mc is a x86 decompiler project for Windows. It's in a very early stage.

The goal of this project is to be able to decompile an executable file into C++ code
that can be built with Visual Studio to produce a functionally identical .exe file.

unwind-mc uses udis86 via PInvoke for linear disassembly and PeNet for PE file parsing.

# Minimal viable product

To achieve the MVP, the tool must be able to decompile one particular late 90s-era .exe file of medium size (~250k instructions)
into C++ code that is functionally identical or can be made identical in reasonable time.
Almost all of the testing is done against this file.

## Current status

Object file disassembly and function finding:
 - ~98% of instructions have an assigned function
 - <1% of instructions have an assigned function whose body was not fully found
 - 2% of instructions don't have an assigned function and/or are improperly disassambled

External symbol resolving:
 - Calls to external functions are resolved for all resolved functions

Coming up next:
 - Transformation of function bodies into simplified flow graph
