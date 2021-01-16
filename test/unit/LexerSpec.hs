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
      