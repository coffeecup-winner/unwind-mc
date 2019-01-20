# What is unwind-mc

unwind-mc is a x86 decompiler project. It's in a very early stage.

The goal of this project is to be able to decompile an executable file into C++ code that can be built to produce a functionally identical .exe file.

unwind-mc uses udis86 for linear disassembly.

## Current status

Implemented decompiler phases:
 1. Disassembly
 2. Function finding
 3. External function call resolution
 4. Decompilation of assembly into a simplified intermediate language
 5. Control flow analysis
 6. Variable assignment and type resolution
 7. Abstract syntax tree building
 8. C++ code generation

Phases 1-3 work reasonably well, with up to 98% instructions decompiled into resolved functions in some medium-sized (~250k instuctions) binary files.

Features and supported instructions are progressively added to phases 4-8 on an as-needed basis for each new function.

No data structure analysis is performed yet.
