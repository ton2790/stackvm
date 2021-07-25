# StackVM

## Info

This project combines a stack-based virtual machine for interpretin bytecode, and
an assmebler for the machine.

The instruction set is as follows:

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
call label args | see calling convention
ret             | see caalling convention