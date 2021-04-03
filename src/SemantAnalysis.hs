module SemantAnalysis where

import SemChkSyntax

{-
Either an empty sequence (no more statements), a sequence of statements, or
a branching point
-}
data CtrlFlowGraph = 
    Empty 
  | Seq Bool CtrlFlowGraph 
  | Branch CtrlFlowGraph CtrlFlowGraph


-- We create the CFG from a list of semantically checked statements
genCFG :: [SStatement] -> CtrlFlowGraph
genCFG [] = Empty
genCFG (s:ss) = case s of
  SReturn _ -> Seq True (genCFG ss) -- found return statement
  SIf _ cons alt -> Branch (genCFG (cons : ss)) (genCFG (alt:ss))
  SDoWhile _ stmnt -> Seq False (genCFG (stmnt:ss))
  SBlock stmnts -> genCFG (stmnts <> ss)
  _ -> Seq False (genCFG ss)


-- Will return true if all branches of the graph end in return
validate :: CtrlFlowGraph -> Bool
validate = \case
  Empty -> False
  Seq b Empty -> b
  Seq _ rest -> validate rest
  Branch left right -> validate left && validate right
