module Parser
  ( Parser.parse
  ) where

import Asm

import Data.Void
import Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space1
symbol :: String -> Parser String
symbol = L.symbol space1
integer :: Parser Int
integer = lexeme L.decimal
signed :: Parser Int
signed = L.signed (return ()) integer

jumpLabel :: Parser String
jumpLabel = some alphaNumChar

parse :: String -> Maybe [Asm]
parse s = case runParser (many parseLine) "" s of
  Right xs -> Just xs
  Left _   -> Nothing

parseLine :: Parser Asm
parseLine = try (Asm.Label <$> (jumpLabel <* symbol ":"))
            <|> try (symbol "nop" *> pure Nop)
            <|> try (symbol "add" *> pure Add)
            <|> try (symbol "sub" *> pure Sub)
            <|> try (symbol "mul" *> pure Mul)
            <|> try (symbol "lt" *> pure Lt)
            <|> try (symbol "eq" *> pure Eq)
            <|> try (Jmp <$> (symbol "j" *> lexeme jumpLabel))
            <|> try (Jnz <$> (symbol "jnz" *> lexeme jumpLabel))
            <|> try (Jz <$> (symbol "jz" *> lexeme jumpLabel))
            <|> try (Lit <$> (symbol "lit" *> signed))
            <|> try (Load <$> (symbol "load" *> signed))
            <|> try (Store <$> (symbol "store" *> signed))
            <|> try (GLoad <$> (symbol "gload" *> signed))
            <|> try (GStore <$> (symbol "gstore" *> signed))
            <|> try (symbol "print" *> pure Print)
            <|> try (symbol "pop" *> pure Pop)
            <|> try (symbol "halt" *> pure Halt)
            <|> try (do { symbol "call"
                        ; addr <- lexeme jumpLabel
                        ; n <- signed
                        ; return $ Call addr n } )
            <|> symbol "ret" *> pure Ret
            
                        
  
