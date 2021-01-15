module Syntax where

type Name = String

data Expr
  = Float    Double
  | Int      Int
  | BinOp    Op Expr Expr
  | Var      String
  | Negation Expr
  | Call     Name [Expr]
  | Function Name [Expr] Expr
  | Extern   Name [Expr]
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)