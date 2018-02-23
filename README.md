# lc3b

An instruction-level assembler/simulator for the LC-3b ISA.

## Overview
The LC-3b is a toy instruction set architecture developed by Yale Patt and
Sanjay Patel for their book, "Introduction to Computing Systems: From Bits and
Gates to C and Beyond" (McGraw-Hill, 2003). The book actually uses a version of
the ISA called LC-3, which is word-addressable. The LC-3b is the
byte-addressable variant.

LC-3b is a 16-bit architecture; all the registers are 2 bytes wide. The full
specification lives
[here](http://users.ece.utexas.edu/~patt/17s.460N/handouts/new_byte.pdf).

This project consists of a simple assembler and simulator for the LC-3b written
in Haskell. I did it just for the sake of doing it, but hopefully it's useful to
somebody.

## Building

This is a Haskell project with a few dependencies, all of which are on
hackage. I use stack to build it. If you have stack installed, you should be
able to run

```
stack build
```

Alternatively, you can run

```
cabal build
```

I also recommend running
```
stack test
```
which assembles and simulates all the test programs, and checks the final
register state to make sure the thing ran correctly. You can actually write your
own tests and insert them into the tests/ directory in the same format as the
other tests that are already in there.

## Running
This project compiles two executables, lc3b-asm and lc3b-sim. lc3b-asm takes a
single assembly file as a command line argument, assembles it, and outputs a
file with a .out extension right next to the input file. There are no command
line switches or anything. Same goes for lc3b-sim -- essentially, you feed that
program the assembled .out file, and it will simulate it, outputting the final
state of the PC and register file upon completion. If you want to see the state
of the memory, you are out of luck unless you are willing to hack on the
sim/Main.hs code yourself. 

## Random notes (mainly for myself)

- TODO: Enhance simulator to accept multiple object files. Use the first one as
  the program (so the PC gets set to the entry point in that file). The rest of
  the object files just get loaded into the memory as data files.
