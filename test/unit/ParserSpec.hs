-- {-# LANGUAGE OverloadedStrings #-}
-- https://stackoverflow.com/questions/43263965/how-to-run-multiple-test-files-with-haskell-stack-project

module ParserSpec where

import Control.Applicative hiding (some)
import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Pikkuc as P

type Parser = Parsec Void Text

myAParser :: Parser String
myAParser = some (char 'a')

spec :: Spec
spec = do
  -- -- tutorial examples
  -- describe "tutorial examples" $ do
  --   it "returns correct result" $
  --     parse myAParser "" "aaa" `shouldParse` "aaa"
  --   it "result of parsing satisfies what it should" $
  --     parse myAParser "" "aaaa" `parseSatisfies` ((== 4) . length)
  --   it "should parse 'a's all right" $
  --     parse myAParser "" `shouldSucceedOn` "aaaa"
  --   it "should fail on 'b's" $
  --     parse myAParser "" `shouldFailOn` "bbb"
  
  describe "Expression terms" $ do
    it "parses integer literals" $
      parse P.pTerm "" "2" `shouldParse` P.Literal 2
    it "parses character literals" $
      parse P.pTerm "" "'a'" `shouldParse` P.CharLit 97
    it "parses string literals" $
      parse P.pTerm "" "\"aaa\"" `shouldParse` P.StrLit "aaa"
    it "parses bool literals" $
      parse P.pTerm "" "true" `shouldParse` P.BoolLit True
    it "parses identifiers" $
      parse P.pTerm "" "varX" `shouldParse` P.Id "varX"

  describe "Data types" $ do
    it "parses 'int 2' as integer" $
      parse P.pType "" "int 2" `shouldParse` P.TyInt
    it "doesn't parse 'int 2' as bool" $
      parse P.pType "" "int 2" `parseSatisfies` (/= P.TyBool)
    it "parses 'float 2' as floating point number" $
      parse P.pType "" "float 2" `shouldParse` P.TyFloat
    -- should pass once type checking is implemented
    -- it "fails on parsing erroneous floating point numbers" $
    --   parse P.pType  "" `shouldFailOn` "float 'a'" 
    it "parses \"char 'a'\" as char" $
      parse P.pType "" "char 'a'" `shouldParse` P.TyChar
    it "parses 'bool False' as boolean type" $
      parse P.pType "" "bool False" `shouldParse` P.TyBool 

  describe "Variable declarations" $ do
    it "parses 'int x;'" $
      parse P.pVarDec "" `shouldSucceedOn` "int x;"
    it "fails on 'int x' (missing semicolon)" $
      parse P.pVarDec "" `shouldFailOn` "int x" 
    it "fails on 'int;' (missing identifier)" $
      parse P.pVarDec "" `shouldFailOn` "int;"
    it "parses 'float y;'" $
      parse P.pVarDec "" `shouldSucceedOn` "float y;"
    it "parses 'char k;'" $
      parse P.pVarDec "" `shouldSucceedOn` "char k;"
    it "fails on 'int bool;' (reserved word)" $
      parse P.pVarDec "" `shouldFailOn` "int bool;"

  describe "Expression parsing" $ do
    it "parses 'x = 1' successfully" $
      parse P.pExpr "" `shouldSucceedOn` "x = 1"
    it "parses 'x = 100' correctly" $
      parse P.pExpr "" "x = 100" `shouldParse`
        P.Assign (P.Id "x") (P.Literal 100)
    it "fails on 'x ='" $
      parse P.pExpr "" `shouldFailOn` "x ="

  describe "Statement parsing" $ do
    it "parses 'return;' correctly" $
      parse P.pStatement "" "return;" `shouldParse`
        P.Return P.Noexpr
    it "fails on 'return' (missing semicolon)" $
      parse P.pStatement "" `shouldFailOn` "return"
    it "parses block statements correctly" $
      parse P.pStatement "" "{x = 1; return; }" `shouldParse`
        P.Block [P.Expr (P.Assign (P.Id "x") (P.Literal 1)),P.Return P.Noexpr]