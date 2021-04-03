module Syntax where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Char (chr)

-- Binary ops
data Op
  = Add
  | Sub
  | Mult
  | Div
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  deriving (Eq, Ord, Show)

-- Unary ops
data Uop = Neg
         | Not
         deriving (Show, Eq)

data Expr
  = Literal      Int
  | StrLit       Text
  | CharLit      Int       -- chars reduce to ints in codegen
  | FloatLiteral Double
  | BoolLit      Bool
  | Null
  | Id           Text      -- variable names
  | Binop        Op Expr Expr
  | Unop         Uop Expr
  | Call         Text [Expr]
  | Cast         Type Expr
  | Assign       Expr Expr
  | Noexpr
  deriving (Eq, Show)

data Statement 
  = Expr   Expr
  | Block [Statement]
  | Return Expr
  | If     Expr Statement Statement
  | While  Expr Statement
  deriving (Show, Eq)
            
data Type
  = TyInt
  | TyFloat
  | TyChar
  | TyBool
  | TyVoid
  | TyString
  deriving (Show, Eq)

data Bind = Bind { bindType :: Type, bindName :: Text } 
  deriving (Show, Eq)

-- we'll use this later
data Struct = Struct { structName :: Text, structFields :: [Bind] }
  deriving (Show, Eq)

data Function = Function
  { typ  :: Type
  , name :: Text
  , funcargs :: [Bind]
  , locals :: [Bind]
  , body :: [Statement]
  }
  deriving (Show, Eq)

data Program = Program [Bind] [Function] deriving (Eq, Show)

--------------------
-- Pretty instances
--------------------

instance Pretty Type where
  pretty = \case
    TyInt -> "int"
    TyFloat -> "float"
    TyChar -> "char"
    TyBool -> "bool"
    TyVoid -> "void"

instance Pretty Op where
  pretty = \case
    Add -> "+"
    Sub -> "-"
    Mult -> "*"
    Div -> "/"
    Equal -> "=="
    Neq -> "!="
    Less -> "<"
    Leq -> "<="
    Greater -> ">"
    Geq -> ">="

instance Pretty Uop where
  pretty = \case
    Neg -> "-"
    Not -> "!"

instance Pretty Bind where
  pretty (Bind ty nm) = pretty ty <+> pretty nm

instance Pretty Function where
  pretty (Function typ name funcargs locals body) =
    pretty typ <+> pretty name <> tupled (map pretty funcargs)
    <> hardline <> lbrace <> hardline <>
    indent 4 (hardsep (map decl locals ++ map pretty body))
    <> hardline <> rbrace <> hardline

instance Pretty Expr where
  pretty = \case
    Literal i -> pretty i
    FloatLiteral f -> pretty f
    CharLit c -> squotes $ pretty (chr c)
    StrLit s -> dquotes $ pretty s
    BoolLit b -> if b then "true" else "false"
    Id t -> pretty t
    Binop op lhs rhs -> hsep [pretty lhs, pretty op, pretty rhs]
    Unop op e -> pretty op <> parens (pretty e)
    Call f es -> pretty f <> tupled (map pretty es)
    Assign lhs rhs -> pretty lhs <+> "=" <+> pretty rhs
    Noexpr -> mempty

instance Pretty Statement where
  pretty = \case
    Expr e -> pretty e <> semi
    Block ss -> lbrace <> hardline <> indent 4 (vsep (map pretty ss))
      <> hardline <> rbrace
    Return e -> "return" <+> pretty e <> semi
    If pred cons alt ->
      "if" <+> parens (pretty pred) <+> pretty cons <> prettyAlt
        where
          prettyAlt =
            case alt of
              Block [] -> mempty
              _ -> hardline <> "else" <+> pretty alt
    While cond body -> "while" <+> parens (pretty cond) <+> pretty body

instance Pretty Program where
  pretty (Program binds funcs) = hardsep
    (map decl binds ++ map pretty funcs)

decl :: Pretty a => a -> Doc ann
decl bind = pretty bind <> semi

hardsep :: [Doc ann] -> Doc ann
hardsep = concatWith (\x y -> x <> hardline <> y)

