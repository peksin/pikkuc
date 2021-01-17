module Lexer where


import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Char
import           Data.Void ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad
import           Data.String.Conversions

{- 
"the best way to start writing a parser with megaparsec 
is to define a custom type synonym for your parser"
There's a possibility to use a custom error type here
but we just define it as Void and input stream type
as Text. This type synonym saves us the trouble of
defining the same thing for every single parser we make.
This makes type Parser have kind '* -> *' which is much
cleaner
-}

type Parser = Parsec Void Text

{- 
Space consumer for skipping whitespace and c-style comments.
Picks up whitespace and doesn't accept empty so L.space 
doesn't go into an infinite loop.
We don't care about indentation in pikkuc.
-}
sc :: Parser ()
sc = L.space
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

--------------------
-- helpers for paired delimiters and common symbols
--------------------

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

dquotes :: Parser a -> Parser a
dquotes = between (single '"') (single '"')

squotes :: Parser a -> Parser a
squotes = between (single '\'') (single '\'')

semi :: Parser ()
semi = void $ symbol ";"

comma :: Parser ()
comma = void $ symbol ","

-------------------------------
-- reserved words
-------------------------------
rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [Text]
rws =
  [ "if"
  , "then"
  , "else"
  , "while"
  , "true"
  , "false"
  , "int"
  , "bool"
  , "char"
  , "float"
  , "void"
  , "return"
  , "NULL"
  ]

stringLiteral :: Parser Text
stringLiteral = do
  content <- dquotes $ takeWhileP Nothing (/= '"')
  pure $ T.pack (read ('"' : cs content ++ "\""))

{-
ord converts ascii char to its integer representation
first choice doesn't have a backslash or semicolon in it
second choice has a backslash, in which case we expect
an ASCII code for a character
-}
charLiteral :: Parser Int
charLiteral =
  squotes $ (ord <$> satisfy (`notElem` ['\\', '\'']))
         <|> (single '\\' >> int)

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
 where
  p = fmap T.pack $ (:) <$> letterChar
                        <*> many (alphaNumChar <|> single '_')
  check x = if x `elem` rws
    then fail $ "keyword " <> show x <> " cannot be an identifier"
    else return x

int :: Parser Int
int = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

