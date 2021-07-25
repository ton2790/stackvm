# StackVM

## Info

This project combines a stack-based virtual machine for interpretin bytecode, and
an assmebler for the machine.

## Virtual Machine

This machine has a Harvard architecture,
with memory divided into a 64 KiB instruction tape, 64 KiB of RAM, and a 1KiB stack.

There are only three registers: a program counter, a stack pointer, and a frame pointer.

## Instruction Set

Instruction | Action
----------- | -------
nop         | 
add         | stack[sp-1] += stack[sp--]
sub         | stack[sp-1] -= stack[sp--]
mul         | stack[sp-1] *= stack[sp--]
lt          | stack[sp-1] = stack[sp-1] < stack[sp--]
eq          | stack[sp-1] = stack[sp-1] == stack[sp--]
j label     | pc = addr(label)
jnz label   | if (stack[sp--] != 0) pc = addr(label)
jz label    | if (stack[sp--] == 0) pc = addr(label)
lit n       | stack[++sp] = n
load n      | stack[++sp] = stack[fp + n]
store n     | stack[fp+n] = stack[sp--]
gload n     | unimplemented
gstore n    | unimplemented
print       | print(stack[sp--])
pop         | sp--
halt        | terminate VM
call label argc | see calling convention
ret             | see caalling convention

## Calling Convention

When a procedure is called, the argument count, frame pointer, and program counter are pushed to the stack,
and the frame pointer is set to the current stack pointer before jumping to the procedure.

From there, access to local variables is enabled, using the ```load``` and ```store``` instructions.

The local variables are accessed as offsets to the frame pointer as follows:

Offset | Variable Type
------ | -------------
fp-argc ... fp-2 | procedure args
fp-2 | argc
fp-1 | previous frame pointer
fp | previous program counter
fp+1 ... | temporary local variables

When returning from a procedure, it must be the case that ```sp = fp+1```, with the variable at the
top of the stack being the return value.

Upon returning, the previous state is restored, but the arguments atop the stack are replaced by the
return value.

## Use
If you'd like to try the project out,  install cabal and in the base directory of this repository,
run ```cabal run stackvm -- assemble [INFILE] [[-o OUTFILE]]```

You can then execute your bytecode with ```cabal run stackvm -- execute [EXECUTABLE]```.