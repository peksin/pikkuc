module Main where

-- https://www.stephendiehl.com/llvm/
-- https://markkarpov.com/tutorial/megaparsec.html#lexing
-- https://blog.josephmorag.com/posts/mcc3/
-- http://blog.ezyang.com/2013/05/the-ast-typing-problem/


import           Pikkuc hiding ( Parser )
import           Options.Applicative
import           LLVM.Pretty
import           Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import           Text.Pretty.Simple
import           Data.Text.Prettyprint.Doc.Internal
-- import           Prettyprinter.Internal
import           Data.Text.Prettyprint.Doc.Render.Text

data Action = Ast | Sast | LLVM | Compile FilePath | Run

data Options = Options { action :: Action
                       , infile :: FilePath }

pAction :: Parser Action
pAction =
  flag' Ast (long "ast" <> short 'a' <> help "Pretty print the ast")
    <|> flag' Sast (long "sast" <> short 's' <> help "Pretty print the sast")
    <|> flag'
          LLVM
          (long "llvm" <> short 'l' <> help "Pretty print the generated llvm")
    <|> flag' Compile
              (long "compile" <> short 'c' <> help "Compile to an executable")
    <*> strOption (short 'o' <> value "a.out" <> metavar "FILE")
    -- running the file to see the expected output is default
    <|> pure Run

pOptions :: Parser Options
pOptions =
  Options
    <$> pAction
    <*> strArgument (help "Source file" <> metavar "FILE")

main :: IO ()
main = runOpts =<< execParser (pOptions `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    infoString
      = "Run the pikkuc compiler on the given file. \
         \Passing no flags will compile the file, \
         \execute it and print the output."

runOpts :: Options -> IO ()
runOpts (Options action infile) = do
  program <- T.readFile infile
  let parseTree = runParser pProgram infile program
  case parseTree of
    Left  err -> putStrLn $ errorBundlePretty err
    Right ast -> case action of
      Ast -> putDoc $ pretty ast <> pretty (T.pack "\n")
      _   -> error "Not yet implemented"





















-- import Lib

-- import Parser

-- import Control.Monad.Trans
-- import System.Console.Haskeline



-- main :: IO ()
-- main = putStrLn "Hi gaiz"