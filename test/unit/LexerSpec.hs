-- {-# LANGUAGE OverloadedStrings #-}
module LexerSpec where

import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Pikkuc as P

spec :: Spec
spec = do
  describe "Lexer helpers" $ do
    it "recognizes semicolon" $
      parse P.semi "" ";" `shouldParse` ()
    it "recognizes comma" $
      parse P.comma "" "," `shouldParse` ()
    it "fails when expecting semicolon and getting something else" $
      parse P.semi "" `shouldFailOn` ","

  describe "Character literals" $ do
    it "parses 'a' as its ASCII code" $
      parse P.charLiteral "" "'a'" `shouldParse` 97
    it "fails on empty char ''" $
      parse P.charLiteral "" `shouldFailOn` "''"
    it "parses '\\97' correctly as an ASCII code" $ do
      parse P.charLiteral "" "'\\97'" `shouldParse` 97

  describe "String literals" $ do
    it "parses \"testing\" as a string literal" $
      parse P.stringLiteral "" "\"testing\"" `shouldParse` "testing"
    it "fails on a string literal without enclosing double quotes" $
      parse P.stringLiteral "" `shouldFailOn` "\"testing"

  describe "Identifiers" $ do
    it "parses 'testing' as an identifier" $
      parse P.identifier "" "testing" `shouldParse` "testing"
    it "fails on a reserved word as an identifier" $
      parse P.identifier "" `shouldFailOn` "while"
  
  describe "Integers and floating point numbers" $ do
    it "parses a one digit integer" $
      parse P.int "" "5" `shouldParse` 5
    it "parses a multidigit integer" $
      parse P.int "" "43234" `shouldParse` 43234
    it "parses a number with a decimal point in it as integer if trying\
       \ to parse an integer" $
       parse P.int "" "5.99" `shouldParse` 5
    it "parses the floating point number '5.9' correctly" $
      parse P.float "" "5.9" `shouldParse` 5.9
    it "parses the floating point number '14E9898' correctly" $
      parse P.float "" "14E9" `shouldParse` 1.4e10
    it "parses the floating point number '7e3' correctly" $
      parse P.float "" "7e3" `shouldParse` 7000.0
    -- https://hackage.haskell.org/package/megaparsec-9.0.1/docs/Text-Megaparsec-Char-Lexer.html
    -- megaparsec doesn't parse plain ints as floats since v. 6.1.1!!!
    it "doesn't parse plain integers as floats" $
      parse P.float "" `shouldFailOn` "5"
    
      