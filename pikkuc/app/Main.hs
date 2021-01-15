module Main where

-- this works as a repl for now
-- https://www.stephendiehl.com/llvm/
-- https://markkarpov.com/tutorial/megaparsec.html#lexing
-- https://blog.josephmorag.com/posts/mcc3/

import Lib

import Parser

import Control.Monad.Trans
import System.Console.Haskeline

-- process :: String -> IO ()
-- process line = do
--   let res = parseToplevel line
--   case res of
--     Left err -> print err
--     Right ex -> mapM_ print ex

-- main :: IO ()
-- main = runInputT defaultSettings loop
--   where
--     loop = do
--       minput <- getInputLine "ready> "
--       case minput of
--         Nothing -> outputStrLn "Goodbye."
--         Just input -> liftIO (process input) >> loop

main :: IO ()
main = putStrLn "Hi gaiz"