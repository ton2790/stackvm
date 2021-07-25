module Opcodes where

data Op = Nop
        | Add
        | Sub
        | Mul
        | Lt
        | Eq
        | Jmp
        | Jnz
        | Jz
        | Lit
        | Load
        | Store
        | GLoad
        | GStore
        | Print
        | Pop
        | Halt
        | Call
        | Ret
        deriving (Eq, Ord, Show, Read, Enum, Bounded)
