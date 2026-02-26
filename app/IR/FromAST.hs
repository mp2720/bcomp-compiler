{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module IR.FromAST (convert) where

import qualified AST as A
import Control.Lens.Basic (Lens)
import qualified Control.Lens.Basic as Lens
import Control.Monad (forM_, void)
import Control.Monad.Trans.State (State, execState, get, modify, put, runState)
import Diagnostics (Diagnostic (..))
import IR
import IR.Scope (Scope)
import qualified IR.Scope as Scope
import Text.Printf (printf)

data Pass = Pass
  { -- | Reversed
    diagnostics :: [Diagnostic],
    labelsScope :: Scope (),
    varsScope :: Scope VarKind,
    vars :: [(FlatIdent, VarKind)],
    labels :: [(FlatIdent, ())],
    -- | Reversed
    instrs :: [LinearInstr]
  }

-- | If diagnostics list has errors, then the program is malformed.
-- And the output could be bogus only if the program is malformed.
convert :: A.Program -> Either [Diagnostic] LinearProgram
convert ast =
  case diagnostics pass of
    [] ->
      Right $
        LinearProgram
          { progInstrs = reverse $ instrs pass,
            progVars = vars pass,
            progLabels = map fst $ labels pass
          }
    diagns -> Left $ reverse diagns
  where
    (_, pass) =
      runState
        ( do
            pass1Program ast
            pass2Program ast
        )
        Pass
          { diagnostics = [],
            labelsScope = Scope.empty,
            varsScope = Scope.empty,
            vars = [],
            labels = [],
            instrs = []
          }

-- General helpers

reportDiagn :: Diagnostic -> State Pass ()
reportDiagn d = modify $ \p -> p {diagnostics = d : diagnostics p}

emit :: LinearInstr -> State Pass ()
emit instr = modify $ Lens.over $(Lens.field 'instrs) (instr :)

emitSeq :: SeqInstr -> State Pass ()
emitSeq = emit . LSeqInstr

emitBranch :: BranchInstr -> State Pass ()
emitBranch = emit . LBranchInstr

emitLabelHere :: Label -> State Pass ()
emitLabelHere = emit . PlaceLabel

-- Symbol helpers

type Symbols s = [(FlatIdent, s)]

type ScopeLens s = Lens Pass Pass (Scope s) (Scope s)

type SymbolsLens s = Lens Pass Pass (Symbols s) (Symbols s)

declSymbol :: ScopeLens s -> SymbolsLens s -> Maybe (A.P, A.Ident) -> s -> State Pass FlatIdent
declSymbol scopeLens symbolsLens mbPosIdent s = do
  state <- get
  let scope = Lens.view scopeLens state
  case mbPosIdent of
    Just (pos, ident) -> do
      case Scope.lookupSymbol scope ident of
        Just _ -> do
          reportDiagn $ Error pos (printf "symbol %s is redeclared" ident)
        Nothing -> return ()
    Nothing -> return ()
  let (flatId, modifiedScope) = Scope.declSymbol scope (fmap snd mbPosIdent) s
  modify $ Lens.set scopeLens modifiedScope
  modify $ Lens.over symbolsLens ((flatId, s) :)
  return flatId

declLabel :: Maybe (A.P, A.Ident) -> State Pass FlatIdent
declLabel mbPosIdent = declSymbol $(Lens.field 'labelsScope) $(Lens.field 'labels) mbPosIdent ()

declVar :: Maybe (A.P, A.Ident) -> VarKind -> State Pass FlatIdent
declVar = declSymbol $(Lens.field 'varsScope) $(Lens.field 'vars)

resolveSymbol :: ScopeLens s -> s -> A.Ident -> A.P -> State Pass (FlatIdent, s)
resolveSymbol scopeLens defaultS ident pos = do
  state <- get
  case Scope.lookupSymbolRec (Lens.view scopeLens state) ident of
    Nothing -> do
      reportDiagn $ Error pos (printf "unknown symbol %s" ident)
      return (BogusIdent ident, defaultS)
    Just entry -> return entry

resolveVar :: A.Ident -> A.P -> State Pass (FlatIdent, VarKind)
resolveVar = resolveSymbol $(Lens.field 'varsScope) BogusKind

resolveLabel :: String -> A.P -> State Pass FlatIdent
resolveLabel ident pos = fst <$> resolveSymbol $(Lens.field 'labelsScope) () ident pos

-- Pass 1 (collect labels)

pass1Program :: A.Program -> State Pass ()
pass1Program (A.Program stms) = forM_ stms pass1Stmt

pass1Stmt :: A.Stmt -> State Pass ()
pass1Stmt (A.ExecStmt _ (A.LabeledStmt pos mbIdent execStmt)) = do
  case mbIdent of
    Nothing -> pure ()
    Just ident -> void $ declLabel (Just (pos, ident))
  case execStmt of
    A.Block _ stmts -> forM_ stmts pass1Stmt
    _ -> return ()
pass1Stmt _ = return ()

-- Pass 2 (collect variables & emit IR)

pass2Program :: A.Program -> State Pass ()
pass2Program (A.Program stmts) = forM_ stmts pass2Stmt

pass2Stmt :: A.Stmt -> State Pass ()
pass2Stmt (A.ExecStmt _ stmt) = pass2LabeledStmt stmt
pass2Stmt (A.VarDef pos ident mbRexpr) = do
  flatId <- declVar (Just (pos, ident)) Scalar
  forM_ mbRexpr (pass2AssignToVar flatId)
pass2Stmt (A.ArrDef pos ident mbExplicitSize elements) = do
  let lenElements = fromIntegral $ length elements
  size <- case mbExplicitSize of
    Nothing -> return lenElements
    Just explicitSize -> do
      if explicitSize < lenElements
        then do
          reportDiagn $ Error pos ""
          return lenElements
        else
          return explicitSize

  let elementsPadded = take (fromIntegral size) (elements ++ repeat 0)

  flatId <- declVar (Just (pos, ident)) (Array elementsPadded)
  modify $ Lens.over $(Lens.field 'vars) ((flatId, Array elementsPadded) :)

pass2LabeledStmt :: A.LabeledStmt -> State Pass ()
pass2LabeledStmt (A.LabeledStmt pos mbIdent stmt) = do
  case mbIdent of
    Just ident -> do
      label <- resolveLabel ident pos
      emitBranch $ Branch $ Label label
      emitLabelHere $ Label label
    Nothing -> pure ()
  pass2ExecStmt stmt

pass2ExecStmt :: A.ExecStmt -> State Pass ()
pass2ExecStmt (A.Assign _ lhs rhs) = pass2Assign lhs rhs
pass2ExecStmt (A.If _ cond then_ else_) =
  pass2If cond (pass2LabeledStmt then_) (pass2LabeledStmt else_)
pass2ExecStmt (A.Goto pos label) = do
  flatLabel <- resolveLabel label pos
  emitBranch $ Branch $ Label flatLabel
  return ()
pass2ExecStmt (A.Block _ stmts) = do
  state <- get
  let pVarScope = varsScope state

  let chVarScope = Scope.startChild pVarScope
  let stateUpd = execState (forM_ stmts pass2Stmt) state {varsScope = chVarScope}
  let parentVarScopeUpd = Scope.endChild pVarScope chVarScope

  put stateUpd {varsScope = parentVarScopeUpd}
pass2ExecStmt (A.NoOp _) = return ()

pass2Assign :: A.LeftExpr -> A.RightExpr -> State Pass ()
pass2Assign (A.PtrDeref _ dstExpr) srcExpr = do
  -- \*dst = src
  dst <- pass2RightExpr dstExpr
  src <- pass2RightExpr srcExpr
  emitSeq $ Store dst src
pass2Assign (A.Var varPos varId) srcExpr = do
  (varFlatId, varKind) <- resolveVar varId varPos
  expectScalar varId varPos varKind
  pass2AssignToVar varFlatId srcExpr

-- | Assign to scalar var.
pass2AssignToVar :: FlatIdent -> A.RightExpr -> State Pass ()
pass2AssignToVar varFlatId rexpr = do
  -- in case of unary operator redundant copy instruction is inserted
  -- TODO: OPT: optimize out redundant copy

  src <- pass2RightExpr rexpr
  emitSeq $ Copy varFlatId src

pass2If :: A.RightExpr -> State Pass () -> State Pass () -> State Pass ()
pass2If cond then_ else_ = do
  labelThenId <- declLabel Nothing
  let labelThen = Label labelThenId
  labelElseId <- declLabel Nothing
  let labelElse = Label labelElseId
  labelIfEndId <- declLabel Nothing
  let labelIfEnd = Label labelIfEndId

  -- TODO: OPT: a lot of redundant fallthrough labels and branches are emitted for nested ifs.

  emitCond cond labelThen labelElse
  emitLabelHere labelThen
  ( do
      then_
      emitBranch $ Branch labelIfEnd
    )
  emitLabelHere labelElse
  ( do
      else_
      emitBranch $ Branch labelIfEnd
    )
  emitLabelHere labelIfEnd
  where
    emitCond (A.RelOpApp _ leftExpr op rightExpr) labelThen labelElse = do
      l <- pass2RightExpr leftExpr
      r <- pass2RightExpr rightExpr
      emitBranch $
        ( case op of
            A.Equals -> BranchIfEq l r
            A.NotEq -> flip $ BranchIfEq l r
            A.Gt -> flip $ BranchIfLt l r
            A.Geq -> BranchIfGe l r
            A.Lt -> BranchIfLt l r
            A.Leq -> flip $ BranchIfGe l r
            A.UnsignedGt -> flip $ BranchIfUnsignedLt l r
            A.UnsignedGeq -> BranchIfUnsignedGe l r
            A.UnsignedLt -> BranchIfUnsignedLt l r
            A.UnsignedLeq -> flip $ BranchIfUnsignedGe l r
        )
          labelThen
          labelElse
    emitCond condExpr labelThen labelElse = do
      condOpnd <- pass2RightExpr condExpr
      emitBranch $ BranchIfZero condOpnd labelElse labelThen

pass2RightExpr :: A.RightExpr -> State Pass Operand
pass2RightExpr rexpr = do
  -- tmp var could be not referenced at all
  -- TODO: OPT: optimize tmpVar out if not used

  tmpVarFlatId <- declVar Nothing Scalar
  p rexpr tmpVarFlatId
  where
    p (A.Literal _ lit) _ =
      return $ Const lit
    p (A.BinOpApp _ leftExpr astOp rightExpr) tmpVar = do
      left <- pass2RightExpr leftExpr
      right <- pass2RightExpr rightExpr
      let irOp = case astOp of
            A.Add -> Add
            A.Sub -> Sub
            A.BitAnd -> BitAnd
            A.BitOr -> BitOr
      emitSeq $ BinOp tmpVar left irOp right
      return $ Var tmpVar
    p condExpr@(A.RelOpApp {}) tmpVar = do
      -- TODO: OPT: make optimized code for case when logic expression is evaluated to var,
      -- TODO: then used in an if statemtn
      pass2If
        condExpr
        (emitSeq $ Copy tmpVar (Const 1))
        (emitSeq $ Copy tmpVar (Const 0))
      return $ Var tmpVar
    p (A.UnaryOpApp _ astOp expr) tmpVar = do
      opnd <- pass2RightExpr expr
      emitSeq $ case astOp of
        A.BitNot -> BitNot tmpVar opnd
        A.Negate -> Negate tmpVar opnd
      return $ Var tmpVar
    p (A.AddressOf _ (A.Var varPos varId)) _ = do
      (varFlatId, _) <- resolveVar varId varPos
      return $ Address varFlatId
    p (A.AddressOf _ (A.PtrDeref _ ptrExpr)) tmpVar =
      p ptrExpr tmpVar
    p (A.Sizeof pos arrId) _ = do
      (_, arrKind) <- resolveVar arrId pos
      elements <- expectArray arrId pos arrKind
      return $ Const $ fromIntegral (length elements)
    p (A.LeftExpr _ l) tmpVar =
      lexpr l tmpVar

    lexpr (A.Var pos varId) _ = do
      (varFlatId, varKind) <- resolveVar varId pos
      return $ case varKind of
        Array _ -> Address varFlatId
        Scalar -> Var varFlatId
        BogusKind -> Var varFlatId
    lexpr (A.PtrDeref _ ptrExpr) tmpVar = do
      src <- pass2RightExpr ptrExpr
      emitSeq $ Load tmpVar src
      return $ Var tmpVar

expectArray :: A.Ident -> A.P -> VarKind -> State Pass [Integer]
expectArray ident pos Scalar = do
  reportDiagn (Error pos $ printf "Variable %s has scalar kind, but an array was expected" ident)
  return []
expectArray _ _ (Array elements) = pure elements
expectArray _ _ BogusKind = pure []

expectScalar :: A.Ident -> A.P -> VarKind -> State Pass ()
expectScalar ident pos (Array _) =
  reportDiagn (Error pos $ printf "Variable %s has array kind, but a scalar was expected" ident)
expectScalar _ _ _ = pure ()
