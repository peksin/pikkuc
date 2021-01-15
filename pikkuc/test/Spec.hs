{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative hiding (some)
import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

myAParser :: Parser String
myAParser = some (char 'a')

main :: IO ()
main = hspec $
  describe "myAParser" $ do
    it "returns correct result" $
      parse myAParser "" "aaa" `shouldParse` "aaa"
    it "result of parsing satisfies what it should" $
      parse myAParser "" "aaaa" `parseSatisfies` ((== 4) . length)
    it "should parse 'a's all right" $
      parse myAParser "" `shouldSucceedOn` "aaaa"
    it "should fail on 'b's" $
      parse myAParser "" `shouldFailOn` "bbb"