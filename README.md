# lc3b

An instruction-level assembler/simulator for the LC-3b ISA.

## Current plan

First I want to get the simulator going; set up the various data structures, and
get it to a point where I can manually code up a simple program and watch it
go. I'll hand-code the program in a Haskell file and Main will just run that
program against the initial LC-3b state. Once it terminates, we want to just
print all the state changes that were made (maybe omit the memory... heh).

After that, I'll worry about the assembler.
# lc3b
