module Parser ( pProgram, runParser, errorBundlePretty, pTerm
              , pType, pVarDec, pExpr, pStatement) where

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Control.Applicative (liftA2, liftA3)
import Data.String.Conversions
import Data.Either
import Lexer
import Syntax


-- set precedence of operations
-- every item in an inner list has equal precedence
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ unary (Unop Neg) "-"
    , unary (Unop Not) "!"
    ]
  , [ infixL Mult "*", infixL Div "/" ]
  , [ infixL Add "+", infixL Sub "-" ]
  , [ InfixR $ Assign <$ symbol "="]
  ]
  where
    unary op sym = Prefix $ foldr1 (.) <$> some (op <$ symbol sym)
    infixL op sym = InfixL $ Binop op <$ symbol sym
    -- infixR op sym = InfixR $ Binop op <$ symbol sym

pTerm :: Parser Expr
pTerm = parens pExpr
    <|> try (Fliteral <$> float)
    <|> Literal <$> int
    <|> CharLit <$> charLiteral
    <|> StrLit  <$> stringLiteral
    <|> BoolLit <$> (True <$ rword "true" <|> False <$ rword "false")
    <|> Id <$> identifier

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

maybeExpr :: Parser Expr
maybeExpr = option Noexpr pExpr

pType :: Parser Type
pType = TyInt <$ rword "int"
     <|> TyFloat <$ rword "float"
     <|> TyChar  <$ rword "char"
     <|> TyBool  <$ rword "bool"

-- variable declaration
-- currently can't declare and assign on one line
pVarDec :: Parser Bind
pVarDec = Bind <$> pType <*> identifier <* semi

pStatement :: Parser Statement
pStatement = Expr <$> pExpr <* semi
  <|> Return <$> (rword "return" *> maybeExpr <* semi)
  <|> Block  <$> braces (many pStatement)
  <|> pIf
  <|> pWhile

pIf :: Parser Statement 
pIf = liftA3 If (rword "if" *> parens pExpr) pStatement maybeElse
  where maybeElse = option (Block []) (rword "else" *> pStatement)

pWhile :: Parser Statement
pWhile = liftA2 While (rword "while" *> parens pExpr) pStatement

-- function declaration
pFuncDec :: Parser Function
pFuncDec = Function <$> pType <*> identifier <*> pFormals
  <*> (symbol "{" *> many pVarDec)
  <*> (many pStatement <* symbol "}")

pFormals :: Parser [Bind]
pFormals = parens $ pFormal `sepBy` comma
  where pFormal = liftA2 Bind pType identifier 

pProgram :: Parser Program
pProgram = between sc eof $ do
  globals <- many $ try pVarDec -- without try infinite loop of parsing vars!
  Program globals <$> many pFuncDec