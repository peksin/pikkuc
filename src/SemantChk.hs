{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- i'll deal with these later

module SemantChk where

import           Syntax
import           SemChkSyntax
import           SemantChkError
import           SemantAnalysis
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.List (find, findIndex)

type Vars = M.Map (Text, VarKind) Type
type Funcs = M.Map Text Function
data Env = Env { vars  :: Vars
               , funcs :: Funcs }

type Semant = ExceptT SemantError (State Env)

checkBinds :: VarKind -> BindingLoc -> [Bind] -> Semant [Bind]
checkBinds kind loc binds = do
  forM binds $ \case
    Bind TyVoid name -> throwError $ IllegalBinding name Void kind loc

    Bind ty name -> do
      vars <- gets vars
      when (M.member (name, kind) vars)
        $ throwError (IllegalBinding name Duplicate kind loc)
      modify $ \env -> env { vars = M.insert (name, kind) ty vars }
      pure $ Bind ty name
    
checkExpr :: Expr -> Semant SExpr
checkExpr expr = case expr of
  Literal i -> pure (TyInt, SLiteral i)
  FloatLiteral f -> pure (TyFloat, SFloatLiteral f)
  BoolLit b -> pure (TyBool, SBoolLit b)
  CharLit c -> pure (TyChar, SCharLit c)
  StrLit s -> pure (TyString, SStrLit s)
  Null -> pure (TyVoid, SNull)
  Noexpr -> pure (TyVoid, SNoexpr)

  Id s -> do
    vars <- gets vars
    let foundVars = map (\kind -> M.lookup (s, kind) vars) [ Local 
                                                           , FuncArg
                                                           , Global ]
    case join $ find isJust foundVars of
      Nothing -> throwError $ UndefinedSymbol s Var expr
      Just ty -> pure (ty, LVal $ SId s)
  
  Binop op lhs rhs -> do
    lhs'@(t1, _) <- checkExpr lhs
    rhs'@(t2, _) <- checkExpr rhs

    let assertSym = unless (t1 == t2) $ throwError 
                                      $ TypeError [t1] t2 (Expr expr)
        checkArith = do
          unless (isNumeric t1)
            $ throwError (TypeError [TyInt, TyFloat] t1 (Expr expr))
          pure (t1, SBinop op lhs' rhs')

    case op of
      Add ->
        let sexpr = SBinop Add lhs' rhs'
        in case (t1, t2) of
          (TyInt, TyInt) -> pure (TyInt, sexpr)
          (TyFloat, TyFloat) -> pure (TyFloat, sexpr)
          _ -> throwError $ TypeError [TyVoid, TyInt, TyFloat] t1 (Expr expr)
      
      Sub ->
        let sexpr = SBinop Sub lhs' rhs'
        in case (t1, t2) of
          (TyInt, TyInt) -> pure (TyInt, sexpr)
          (TyFloat, TyFloat) -> pure (TyFloat, sexpr)
          _ -> throwError $ TypeError [TyVoid, TyInt, TyFloat] t1 (Expr expr)

      Mult -> assertSym >> checkArith
      Div  -> assertSym >> checkArith

  Unop op e -> do
    e'@(ty, _) <- checkExpr e
    case op of
      Neg -> do
        unless (isNumeric ty)
          $ throwError (TypeError [TyInt, TyFloat] ty (Expr expr))
        pure (ty, SUnop Neg e')
      Not -> do
        unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty (Expr expr)
        pure (ty, SUnop Not e')
  
  Call s es -> do
    funcs <- gets funcs
    case M.lookup s funcs of
      Nothing -> throwError $ UndefinedSymbol s Func expr
      Just f -> do
        es' <- mapM checkExpr es
        -- Check that the correct number of arguments was provided
        let nFuncArgs = length (funcargs f)
            nActuals = length es
        unless (nFuncArgs == nActuals) 
          $ throwError (ArgError nFuncArgs nActuals expr)
        -- Check that types of arguments match
        forM_ (zip (map fst es') (map bindType (funcargs f)))
          $ \(callSite, defSite) ->
              unless (callSite == defSite) $ throwError $ TypeError
                { expected = [defSite]
                , got = callSite
                , errorLoc = Expr expr }
        pure (typ f, SCall s es')

  Cast t' e -> do
    e'@(t, _) <- checkExpr e
    case (t', t) of
      (TyFloat, TyInt) -> pure (t', SCast t' e')
      _                -> throwError $ CastError t' t (Expr expr)

  Assign lhs rhs -> do
    lhs'@(t1, _) <- checkExpr lhs
    rhs'@(t2, _) <- checkExpr rhs
    lval         <- case snd lhs' of
      LVal e -> pure e
      _      -> throwError $ AssignmentError lhs rhs
    case snd rhs' of
      SNull -> checkExpr (Assign lhs (Cast t1 rhs)) 
      _     -> do
        unless (t1 == t2) $ throwError $ TypeError [t1] t2 (Expr expr)
        pure (t2, SAssign lval rhs')
  where
    isNumeric = \case
      TyInt   -> True
      TyFloat -> True
      TyChar  -> True
      _       -> False

checkStatement :: Function -> Statement -> Semant SStatement 
checkStatement func stmnt = case stmnt of
  Expr e -> SExpr <$> checkExpr e

  If pred cons alt -> do
    pred'@(ty, _) <- checkExpr pred
    unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty stmnt
    SIf pred' <$> checkStatement func cons <*> checkStatement func alt
  
  While cond action -> do
    cond'@(ty, _) <- checkExpr cond
    unless (ty == TyBool) $ throwError $ TypeError [TyBool] ty stmnt
    action' <- checkStatement func action
    pure $ SIf cond' (SDoWhile cond' action') (SBlock [])
  
  Return expr -> do
    e@(ty, _) <- checkExpr expr
    unless (ty == typ func) $ throwError $ TypeError [typ func] ty stmnt
    pure $ SReturn e
  
  Block sl -> do
    let flattened = flatten sl
    unless (nothingFollowsRet flattened) $ throwError (DeadCode stmnt)
    SBlock <$> mapM (checkStatement func) flattened
  where
    flatten []             = []
    flatten (Block s : ss) = flatten (s ++ ss)
    flatten (s : ss)       = s : flatten ss

    nothingFollosRet []          = True
    nothingFollowsRet [Return _] = True
    nothingFollowsRet (s : ss)   = case s of
      Return _ -> False
      _        -> nothingFollosRet ss

checkFunction :: Function -> Semant SFunction
checkFunction func = do
  -- add the fname to the table and check for conflicts
  funcs <- gets funcs
  unless (M.notMember (name func) funcs) $ throwError 
                                         $ Redeclaration (name func)
  -- add this func to the symbol table
  modify $ \env -> env { funcs = M.insert (name func) func funcs }

  (funcargs', locals', body') <- locally $ liftM3
    (,,)
    (checkBinds FuncArg (F func) (funcargs func))
    (checkBinds Local (F func) (locals func))
    (checkStatement func (Block $ body func))
  
  case body' of
    SBlock body'' -> do
      unless (typ func == TyVoid || validate (genCFG body''))
        $ throwError (TypeError [typ func] TyVoid (Block $ body func))
      pure $ SFunction { styp = typ func
                       , sname = name func
                       , sfuncargs = funcargs'
                       , slocals = locals'
                       , sbody = SBlock body'' }
    _ -> error "Internal error - block didn't become a block?"

checkProgram :: Program -> Either SemantError SProgram
checkProgram program = evalState (runExceptT (checkProgram' program)) baseEnv
  where 
    baseEnv = Env { vars = M.empty, funcs = M.empty }

    checkProgram' :: Program -> Semant SProgram
    checkProgram' (Program binds funcs) = do
      globals <- checkBinds Global GlobalVar  binds
      funcs' <- mapM checkFunction funcs
      case find (\f -> sname f == "main") funcs' of
        Nothing -> throwError NoMain
        Just _  -> pure (globals, funcs')

locally :: MonadState s m => m a -> m a
locally computation = do
  oldState <- get
  result <- computation
  put oldState
  return result