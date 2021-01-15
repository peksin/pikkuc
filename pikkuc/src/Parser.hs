{-# LANGUAGE OverloadedStrings #-}
-- use ":set -XOverloadedStrings" in REPL
module Parser where

-- import Text.Parsec
-- import Text.Parsec.String (Parser)

-- import qualified Text.Parsec.Expr as Ex
-- import qualified Text.Parsec.Token as Tok

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import Lexer
import Syntax

pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- set precedence of operations
-- every item in an inner list has equal precedence
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "+" id
    ]
  , [ binary "*" (BinOp Times)
    , binary "/" (BinOp Divide)
    ]
  , [ binary "+" (BinOp Plus)
    , binary "-" (BinOp Minus)
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)




-- binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

-- table = [[binary "*" Times Ex.AssocLeft,
--           binary "/" Divide Ex.AssocLeft]
--        , [binary "+" Plus Ex.AssocLeft,
--           binary "-" Minus Ex.AssocLeft]]

-- int :: Parser Expr
-- int = do
--   n <- integer
--   return $ Float (fromInteger n)

-- floating :: Parser Expr
-- floating = do
--   n <- float
--   return $ Float n

-- expr :: Parser Expr
-- expr = Ex.buildExpressionParser table factor

-- variable :: Parser Expr
-- variable = do
--   var <- identifier
--   return $ Var var

-- function :: Parser Expr
-- function = do
--   reserved "def"
--   name <- identifier
--   args <- parens $ many variable
--   body <- expr
--   return $ Function name args body

-- extern :: Parser Expr
-- extern = do
--   reserved "extern"
--   name <- identifier
--   args <- parens $ many variable
--   return $ Extern name args

-- call :: Parser Expr
-- call = do
--   name <- identifier
--   args <- parens $ commaSep expr
--   return $ Call name args

-- factor :: Parser Expr
-- factor = try floating
--       <|> try int
--       <|> try extern
--       <|> try function
--       <|> try call
--       <|> variable
--       <|> parens expr

-- defn :: Parser Expr
-- defn = try extern
--     <|> try function
--     <|> expr

-- contents :: Parser a -> Parser a
-- contents p = do
--   Tok.whiteSpace lexer
--   r <- p
--   eof
--   return r

-- toplevel :: Parser [Expr]
-- toplevel = many $ do
--   def <- defn
--   reservedOp  ";"
--   return def

-- parseExpr :: String -> Either ParseError Expr
-- parseExpr s = parse (contents expr) "<stdin>" s

-- parseToplevel :: String -> Either ParseError [Expr]
-- parseToplevel s = parse (contents toplevel) "<stdin>" s