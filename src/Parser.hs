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


{-
These multiple choice things could be written with an
associative binary operation '<|>' like so:
    TyInt <$ rword "int"
<|> TyFloat <$ rword "float" etc...
but megaparsec's choice just looks more neat
(and admittedly less cool)
-}
pType :: Parser Type
pType = choice
  [ TyInt   <$ rword "int"
  , TyFloat <$ rword "float"
  , TyChar  <$ rword "char"
  , TyBool  <$ rword "bool" ]


{-
We need to have float before int or else every float would get partially parsed as int. FloatLiteral needs to be parsed with try to enable backtracking 
in case of the parser consuming a few ints and THEN failing. The other terms
don't have this ambiguity in them.
-}
pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , try (FloatLiteral <$> float)
  , Literal <$> int
  , CharLit <$> charLiteral
  , StrLit  <$> stringLiteral
  , BoolLit <$> (True <$ rword "true" <|> False <$ rword "false")
  , Id      <$> identifier
  ]


pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


{-
option x p = p <|> pure x
For example, return statement might have an expression 
after it or not (function type being void)
-}
maybeExpr :: Parser Expr
maybeExpr = option Noexpr pExpr
  

{- 
Variable declaration
currently can't declare and assign on one line like:
int x = 42;
-}
pVarDec :: Parser Bind
pVarDec = Bind <$> pType <*> identifier <* semi


pStatement :: Parser Statement
pStatement = choice
  [ Expr <$> pExpr <* semi
  , Return <$> (rword "return" *> maybeExpr <* semi)
  , Block  <$> braces (many pStatement)
  , pIf
  , pWhile
  ]


pIf :: Parser Statement 
pIf = liftA3 If (rword "if" *> parens pExpr) pStatement maybeElse
  where maybeElse = option (Block []) (rword "else" *> pStatement)


pWhile :: Parser Statement
pWhile = liftA2 While (rword "while" *> parens pExpr) pStatement


-- function declaration
pFuncDec :: Parser Function
pFuncDec = Function <$> pType <*> identifier <*> pFuncArgs
  <*> (symbol "{" *> many pVarDec)
  <*> (many pStatement <* symbol "}")

-- parse function arguments
pFuncArgs :: Parser [Bind]
pFuncArgs = parens $ pFuncArg `sepBy` comma
  where pFuncArg = liftA2 Bind pType identifier 


{-
Global variable declarations need to be parsed with try (enable
backtracking), since they share the beginning (type declaration) 
with function declarations.
-}
pProgram :: Parser Program
pProgram = between sc eof $ do
  globals <- many $ try pVarDec
  Program globals <$> many pFuncDec