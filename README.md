# Principles of Functional Programming Final Project
## A compiler for the Jack programming language from nand2tetris

This project is a compiler for the jack programming language as described in the book ["The Elements of Computing Systems"](nand2tetris.org/book) and associated slides (nand2tetris.org/course), using the described compiler architecture as a guideline.

The jack programming language is a simple java like language.  The compiler takes Jack code and compiles it to instructions for a stack based virtual machine.

The compiler also begins to expand the capabilities of the Jack compiler beyond the specification in the following ways:
 * generic classes
 * type checking (rough impmentation complete, but not called from main nor fully stressed tested)
 * lambda expressions (not complete, some sketch and psuedocode present)

### Running the compiler
To run compiler, execute `dune exec final_project` from the project root.
Enter in a directory or file containing the .jack files to be compiled.
The compiler will generate .vm files in place.
The .vm files can be run using the gui launched by `tools/VMEmulator.sh`.
Sample jack programs are in `test_inputs/`.

### Program overview

#### bin/
Contains a single file, main.ml, which serves as a runner the compiler, using the files defined in lib/.
main.ml also contains a couple of test strings for the compiler from development, and a function to run solely the parser.

#### lib/
Contains the bulk of the development work:

jack_ast.ml
: Defines an abstract syntax tree for the jack language, serving as an intermediate between the parser and the compiler.
jack_parser.ml
: Defines a parser for the jack language, using the Angstrom parsing library.
vm_ast.ml
: Defines an abstract syntax tree for the virtual machine language, the target language of the compiler, and provides a commands to translate the ast into string of the concrete syntax.
compilers.ml
: Defines the compiler abstraction, and a variety of helpers and combinators for compilers.  The design was in part inspired by parser combinators.
symbol_table.ml
: A table to hold the data the compiler needs as it walks over the ast to generate the virtual machine code.
type_table.ml
: A table designed to hold information about the types of different functions, for the purposes of type checking jack code.
compilation_engine.ml
: This file ties together the other files into a functional compiler.
io.ml
: a collection of several helper functions for reading and writing files.

#### tools/
Contains tools from the nand2tetris [website](nand2tetris.org/software) to facilitate running the compiled programs and others tools useful in that course.

#### test_inputs/
Contains different jack programs and reference compilations of the programs.

