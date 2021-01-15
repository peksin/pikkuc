{-# LANGUAGE OverloadedStrings #-}
module Lexer where
-- import Text.Parsec.Language (emptyDef)

-- import qualified Text.Parsec.Token as Tok

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- space consumer for skipping whitespace and c-style comments
sc :: Parser ()
sc = L.space
-- picks up whitespace and doesn't accept empty
-- so L.space doesn't go into an infinite loop
  space1 
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- wrapper for lexemes that picks up all trailing whitespace using sc
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parser that matches given text using string internally and
-- picks up all trailing whitespace using sc
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- helpers
charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed (return ()) integer
-- ^ return () == no whitespace allowed between sign and integer

float :: Parser Double
float = lexeme L.float

signedFloat :: Parser Double
signedFloat = L.signed (return ()) float

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy  alphaNumChar)










-- lexer :: Tok.TokenParser ()
-- lexer = Tok.makeTokenParser style
--   where
--     ops = ["+", "*", "-", ";"] -- reserved operations
--     names = ["def", "extern"]  -- reserved names
--     style = emptyDef {
--               Tok.commentLine = "#"
--             , Tok.reservedOpNames = ops
--             , Tok.reservedNames = names
--             }

-- -- integers
-- integer :: Parser Integer
-- integer = Tok.integer lexer

-- -- floating point numbers
-- float :: Parser Double
-- float = Tok.float lexer

-- -- parenthesis
-- parens :: Parser a -> Parser a
-- parens = Tok.parens lexer

-- -- comma separator
-- commaSep :: Parser a -> Parser [a]
-- commaSep = Tok.commaSep lexer

-- -- semicolon separator
-- semiSep :: Parser a -> Parser [a]
-- semiSep = Tok.semiSep lexer

-- -- parses a legal indentifier (symbol name)
-- identifier :: Parser String
-- identifier = Tok.identifier lexer

-- -- parses a reserved name
-- reserved :: String -> Parser ()
-- reserved = Tok.reserved lexer

-- -- parses a reserved operation
-- reservedOp :: String -> Parser ()
-- reservedOp = Tok.reservedOp lexer

