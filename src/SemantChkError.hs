module SemantChkError where

import Syntax
import Data.Text ( Text )
import Data.Text.Prettyprint.Doc
{-
Errors for semantic analysis
-}

type Name = Text

-- is the variable binding in a function or in a global var
data BindingLoc = F Function | GlobalVar
  deriving Show

data SemantError =
    IllegalBinding Name BindingKind VarKind BindingLoc
  | UndefinedSymbol Name SymbolKind Expr
  | TypeError {expected :: [Type], got :: Type, errorLoc :: Statement }
  | CastError { to :: Type, from :: Type, castLoc :: Statement }
  | ArgError { nExpected :: Int, nGot :: Int, callSite :: Expr }
  | Redeclaration Name
  | NoMain
  | AddressError Expr
  | AssignmentError { lhs :: Expr, rhs :: Expr }
  | DeadCode Statement -- unreachable statements
  deriving (Show)

data BindingKind = Duplicate | Void deriving (Show)
data SymbolKind = Var | Func deriving (Show)
data VarKind = Global | FuncArg | Local deriving (Eq, Show, Ord)

instance Pretty VarKind where
  pretty = unsafeViaShow

instance Pretty SymbolKind where
  pretty = \case
    Var -> "variable"
    Func -> "function"

instance Pretty BindingKind where
  pretty = unsafeViaShow

instance Pretty SemantError where
  pretty = \case
    IllegalBinding nm bindKind varKind loc ->
      "Error: Illegal" <+> pretty bindKind <+> pretty varKind <+>
      "binding," <+> pretty nm <+> case loc of
      F f -> "in function" <+> pretty (name f)
      GlobalVar -> mempty
    
    UndefinedSymbol nm symKind expr ->
      "Undefined" <+> pretty symKind <+> pretty nm <+>
      "referenced in:" <> hardline <> pretty expr
    
    TypeError expected got stmnt ->
      "Type error: expected one of" <+> pretty expected <+> "but got"
      <+> pretty got <> ". Error occured in statement:" 
                     <> hardline <> pretty stmnt
    
    CastError to from stmt ->
      "Cast error: can only cast from ints to floats, not from" <+> pretty from
      <+> "to" <+> pretty to <> ". Error occured in statement:" <> hardline <>
      pretty stmt

    ArgError nExpected nGot callSite ->
      "Argument error: function expected" <+> pretty nExpected <+>
      "arguments, but was called with" <+> pretty nGot <+> "arguments"
      <> ". Error occured in call:" <> hardline <> pretty callSite
    
    Redeclaration name -> "Error: redeclaration of function" <+> pretty name

    NoMain -> "Error: main function not defined"

    AssignmentError lhs rhs ->
      "Cannot assign" <+> pretty rhs <+> "to" <+> pretty lhs
    
    AddressError e ->
      "Cannot take address of" <> pretty e
    
    DeadCode stmnt ->
      "Error: nothing may follow a return. Error occured in statement:" <>
      hardline <> pretty stmnt
