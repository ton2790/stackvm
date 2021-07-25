{-# LANGUAGE LambdaCase #-}
module Asm where

import qualified Opcodes as O
import qualified Data.Map as M

type Label = String

data Asm = Nop
         | Add
         | Sub
         | Mul
         | Lt
         | Eq
         | Jmp Label
         | Jnz Label
         | Jz Label
         | Lit Int
         | Load Int
         | Store Int
         | GLoad Int
         | GStore Int
         | Print
         | Pop
         | Halt
         | Call Label Int
         | Ret
         | Label Label
         deriving (Eq, Ord, Show, Read)

mkSymTab :: [Asm] -> M.Map Label Int
mkSymTab xs = go xs M.empty 0 where
  go [] m _ = m
  go (a:as) m n = case a of
    Nop -> go as m (n+1)
    Add -> go as m (n+1)
    Sub -> go as m (n+1)
    Mul -> go as m (n+1)
    Lt -> go as m (n+1)
    Eq -> go as m (n+1)
    Jmp _ -> go as m (n+2)
    Jnz _ -> go as m (n+2)
    Jz _ -> go as m (n+2)
    Lit _ -> go as m (n+2)
    Load _ -> go as m (n+2)
    Store _ -> go as m (n+2)
    GLoad _ -> go as m (n+2)
    GStore _ -> go as m (n+2)
    Print -> go as m (n+1)
    Pop -> go as m (n+1)
    Halt -> go as m (n+1)
    Call _ _ -> go as m (n+3)
    Ret -> go as m (n+1)
    Label l -> go as (M.insert l n m) n


asmToBin :: M.Map Label Int -> Asm -> [Int]
asmToBin m = \case
  Nop -> pure $ fromEnum O.Nop
  Add -> pure $ fromEnum O.Add
  Sub -> pure $ fromEnum O.Sub
  Mul -> pure $ fromEnum O.Mul
  Lt -> pure $ fromEnum O.Lt
  Eq -> pure $ fromEnum O.Eq
  Jmp l -> [fromEnum O.Jmp, get l]
  Jnz l -> [fromEnum O.Jnz, get l]
  Jz l -> [fromEnum O.Jz, get l]
  Lit n -> [fromEnum O.Lit, n]
  Load n -> [fromEnum O.Load, n]
  Store n -> [fromEnum O.Store, n]
  GLoad n -> [fromEnum O.GLoad, n]
  GStore n -> [fromEnum O.GStore, n]
  Print -> pure $ fromEnum O.Print
  Pop -> pure $ fromEnum O.Pop
  Halt -> pure $ fromEnum O.Halt
  Call l n -> [fromEnum O.Call, get l, n]
  Ret -> pure $ fromEnum O.Ret
  Label _ -> []
  where get l = case M.lookup l m of
          Just x -> x

compile :: [Asm] -> [Int]
compile prog = prog >>= asmToBin symtab where
  symtab = mkSymTab prog

