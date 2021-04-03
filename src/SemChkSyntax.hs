module SemChkSyntax where

import Syntax
import Data.Text ( Text )

{-
This is a semantically checked version of the abstract syntax tree
defined in Syntax.hs. Made with the solution "separate IR where nodes
are decorated with types" from here:
http://blog.ezyang.com/2013/05/the-ast-typing-problem/

The 'trick' is the type alias (type SExpr = etc...) that
is a pair holding the expression itself, as well as the type information
for it. Essentially SExpr is the semantically checked version of Expr.
-}

type SExpr = (Type, SExpr')
data SExpr' =
    SLiteral      Int
  | SStrLit       Text
  | SCharLit      Int
  | SFloatLiteral Double
  | SBoolLit      Bool
  | SNull
  | SBinop        Op SExpr SExpr
  | SUnop         Uop SExpr
  | SCall         Text [SExpr]
  | SCast         Type SExpr
  | SAssign       LValue SExpr
  | SNoexpr
  | LVal LValue
  deriving (Eq, Show)


-- https://eli.thegreenplace.net/2011/12/15/understanding-lvalues-and-rvalues-in-c-and-c/
-- struct access will be added later, probably
newtype LValue = SId Text
  deriving (Eq, Show)


{-
Statements don't have the same need for a type alias as expressions
since all statements will reduce to a bunch of expressions anyway
-}
data SStatement =
    SExpr    SExpr
  | SBlock   [SStatement]
  | SReturn  SExpr
  | SIf      SExpr SStatement SStatement
  | SDoWhile SExpr SStatement
  deriving (Eq, Show)


data SFunction = SFunction
  { styp     :: Type
  , sname    :: Text
  , sfuncargs :: [Bind]
  , slocals  :: [Bind]
  , sbody    :: SStatement }
  deriving (Eq, Show)


type SProgram = ([Bind], [SFunction])



