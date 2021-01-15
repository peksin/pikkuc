module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "*", "-", ";"] -- reserved operations
    names = ["def", "extern"]  -- reserved names
    style = emptyDef {
              Tok.commentLine = "#"
            , Tok.reservedOpNames = ops
            , Tok.reservedNames = names
            }

-- integers
integer :: Parser Integer
integer = Tok.integer lexer

-- floating point numbers
float :: Parser Double
float = Tok.float lexer

-- parenthesis
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- comma separator
commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

-- semicolon separator
semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

-- parses a legal indentifier (symbol name)
identifier :: Parser String
identifier = Tok.identifier lexer

-- parses a reserved name
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- parses a reserved operation
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

