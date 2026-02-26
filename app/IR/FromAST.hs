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
convert :: A.Program -> ([Diagnostic], LinearProgram)
convert ast =
  ( reverse $ diagnostics pass,
    LinearProgram
      { progInstrs = reverse $ instrs pass,
        progVars = vars pass,
        progLabels = map fst $ labels pass
      }
  )
  where
    (_, pass) =
      runState
        ( do
            pass1 ast
            pass2 ast
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

emitLabel :: Label -> State Pass ()
emitLabel = emit . PlaceLabel

-- Symbol helpers

type Syms s = [(FlatIdent, s)]

type ScopeLens s = Lens Pass Pass (Scope s) (Scope s)

type SymsLens s = Lens Pass Pass (Syms s) (Syms s)

declSym ::
  ScopeLens s ->
  SymsLens s ->
  Maybe (A.P, A.Ident) ->
  s ->
  State Pass FlatIdent
declSym scopeLens symsLens mbPosIdent s = do
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
  modify $ Lens.over symsLens ((flatId, s) :)
  return flatId

declLabel ::
  Maybe (A.P, A.Ident) ->
  State Pass FlatIdent
declLabel mbPosIdent =
  declSym $(Lens.field 'labelsScope) $(Lens.field 'labels) mbPosIdent ()

declVar ::
  Maybe (A.P, A.Ident) ->
  VarKind ->
  State Pass FlatIdent
declVar =
  declSym $(Lens.field 'varsScope) $(Lens.field 'vars)

resolveSym ::
  ScopeLens s ->
  s ->
  A.Ident ->
  A.P ->
  State Pass (FlatIdent, s)
resolveSym scopeLens defaultS ident pos = do
  state <- get
  case Scope.lookupSymbolRec (Lens.view scopeLens state) ident of
    Nothing -> do
      reportDiagn $ Error pos (printf "unknown symbol %s" ident)
      return (BogusIdent ident, defaultS)
    Just entry -> return entry

resolveVar ::
  A.Ident ->
  A.P ->
  State Pass (FlatIdent, VarKind)
resolveVar = resolveSym $(Lens.field 'varsScope) BogusKind

resolveLabel :: String -> A.P -> State Pass FlatIdent
resolveLabel ident pos = fst <$> resolveSym $(Lens.field 'labelsScope) () ident pos

-- Pass 1 (collect labels)

pass1 :: A.Program -> State Pass ()
pass1 (A.Program stms) = forM_ stms p1Stmt

p1Stmt :: A.Stmt -> State Pass ()
p1Stmt (A.ExecStmt _ stmt) = p1LabeledStmt stmt
p1Stmt _ = return ()

p1LabeledStmt :: A.LabeledStmt -> State Pass ()
p1LabeledStmt (A.LabeledStmt pos mbIdent execStmt) = do
  case mbIdent of
    Nothing -> pure ()
    Just ident -> void $ declLabel (Just (pos, ident))
  case execStmt of
    A.Block _ stmts -> forM_ stmts p1Stmt
    A.If _ _ then_ else_ -> do
      p1LabeledStmt then_
      p1LabeledStmt else_
    _ -> pure ()

-- Pass 2 (collect variables & emit IR)

pass2 :: A.Program -> State Pass ()
pass2 (A.Program stmts) = forM_ stmts p2Stmt

p2Stmt :: A.Stmt -> State Pass ()
p2Stmt stmt = case stmt of
  (A.ExecStmt _ labStmt) -> p2LabeledStmt labStmt
  --
  (A.VarDef pos ident mbRexpr) -> do
    flatId <- declVar (Just (pos, ident)) Scalar
    forM_ mbRexpr (p2AssignToVar flatId)
  --
  (A.ArrDef pos ident mbExplicitSize elements) -> do
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

p2LabeledStmt :: A.LabeledStmt -> State Pass ()
p2LabeledStmt (A.LabeledStmt pos mbIdent stmt) = do
  case mbIdent of
    Nothing -> pure ()
    Just ident -> do
      label <- resolveLabel ident pos
      emitBranch $ Br $ Label label
      emitLabel $ Label label

  p2ExecStmt stmt

p2ExecStmt :: A.ExecStmt -> State Pass ()
p2ExecStmt stmt = case stmt of
  (A.Assign _ lhs rhs) -> p2Assign lhs rhs
  --
  (A.If _ cond then_ else_) ->
    p2If cond (p2LabeledStmt then_) (p2LabeledStmt else_)
  --
  (A.Goto pos label) -> do
    flatLabel <- resolveLabel label pos
    emitBranch $ Br $ Label flatLabel
    return ()
  --
  (A.Block _ stmts) -> do
    state <- get
    let pVarScope = varsScope state

    let chVarScope = Scope.startChild pVarScope
    let stateUpd = execState (forM_ stmts p2Stmt) state {varsScope = chVarScope}
    let pVarScopeUpd = Scope.endChild pVarScope chVarScope

    put stateUpd {varsScope = pVarScopeUpd}
  --
  (A.NoOp _) -> return ()

p2Assign :: A.LeftExpr -> A.RightExpr -> State Pass ()
p2Assign left right = case left of
  (A.PtrDeref _ dstExpr) -> do
    dst <- p2RightExpr dstExpr
    src <- p2RightExpr right
    emitSeq $ Store dst src
  (A.Var varPos varId) -> do
    (varFlatId, varKind) <- resolveVar varId varPos
    expectScalar varId varPos varKind
    p2AssignToVar varFlatId right

-- | Assign to scalar var.
p2AssignToVar :: FlatIdent -> A.RightExpr -> State Pass ()
p2AssignToVar varFlatId rexpr = do
  -- TODO: OPT: optimize redundant copy emitted from assign

  src <- p2RightExpr rexpr
  emitSeq $ Copy varFlatId src

p2If ::
  A.RightExpr ->
  State Pass () ->
  State Pass () ->
  State Pass ()
p2If cond then_ else_ = do
  labelThenId <- declLabel Nothing
  let labelThen = Label labelThenId

  labelElseId <- declLabel Nothing
  let labelElse = Label labelElseId

  labelIfEndId <- declLabel Nothing
  let labelIfEnd = Label labelIfEndId

  -- TODO: OPT: a lot of redundant fallthrough labels and branches are emitted for nested ifs.

  emitCond cond labelThen labelElse
  emitLabel labelThen
  ( do
      then_
      emitBranch $ Br labelIfEnd
    )
  emitLabel labelElse
  ( do
      else_
      emitBranch $ Br labelIfEnd
    )
  emitLabel labelIfEnd
  where
    emitCond (A.RelOpApp _ leftExpr op rightExpr) labelThen labelElse = do
      l <- p2RightExpr leftExpr
      r <- p2RightExpr rightExpr
      emitBranch $
        ( case op of
            A.Equals -> BrIfEq l r
            A.NotEq -> flip $ BrIfEq l r
            A.Gt -> flip $ BrIfLt l r
            A.Geq -> BrIfGe l r
            A.Lt -> BrIfLt l r
            A.Leq -> flip $ BrIfGe l r
            A.UnsignedGt -> flip $ BrIfUnsignedLt l r
            A.UnsignedGeq -> BrIfUnsignedGe l r
            A.UnsignedLt -> BrIfUnsignedLt l r
            A.UnsignedLeq -> flip $ BrIfUnsignedGe l r
        )
          labelThen
          labelElse
    emitCond condExpr labelThen labelElse = do
      condOpnd <- p2RightExpr condExpr
      emitBranch $ BrIfZero condOpnd labelElse labelThen

p2RightExpr :: A.RightExpr -> State Pass Operand
p2RightExpr rexpr = do
  -- TODO: OPT: optimize tmpVar out if not used

  tmpVarFlatId <- declVar Nothing Scalar
  p rexpr tmpVarFlatId
  where
    p (A.Literal _ lit) _ =
      return $ Const lit
    --
    p (A.BinOpApp _ leftExpr astOp rightExpr) tmpVar = do
      left <- p2RightExpr leftExpr
      right <- p2RightExpr rightExpr
      let irOp = case astOp of
            A.Add -> Add
            A.Sub -> Sub
            A.BitAnd -> BitAnd
            A.BitOr -> BitOr
      emitSeq $ BinOp tmpVar left irOp right
      return $ Var tmpVar
    --
    p condExpr@(A.RelOpApp {}) tmpVar = do
      -- TODO: OPT: optimize code for case when logic expression is evaluated to var,
      -- then used in an if statemtn
      p2If
        condExpr
        (emitSeq $ Copy tmpVar (Const 1))
        (emitSeq $ Copy tmpVar (Const 0))
      return $ Var tmpVar
    --
    p (A.UnaryOpApp _ astOp expr) tmpVar = do
      opnd <- p2RightExpr expr
      emitSeq $ case astOp of
        A.BitNot -> BitNot tmpVar opnd
        A.Negate -> Negate tmpVar opnd
      return $ Var tmpVar
    --
    p (A.AddressOf _ (A.Var varPos varId)) _ = do
      (varFlatId, _) <- resolveVar varId varPos
      return $ Address varFlatId
    --
    p (A.AddressOf _ (A.PtrDeref _ ptrExpr)) tmpVar =
      p ptrExpr tmpVar
    --
    p (A.Sizeof pos arrId) _ = do
      (_, arrKind) <- resolveVar arrId pos
      elements <- expectArr arrId pos arrKind
      return $ Const $ fromIntegral (length elements)
    --
    p (A.LeftExpr _ l) tmpVar =
      lexpr l tmpVar

    lexpr (A.Var pos varId) _ = do
      (varFlatId, varKind) <- resolveVar varId pos
      return $ case varKind of
        Array _ -> Address varFlatId
        Scalar -> Var varFlatId
        BogusKind -> Var varFlatId
    --
    lexpr (A.PtrDeref _ ptrExpr) tmpVar = do
      src <- p2RightExpr ptrExpr
      emitSeq $ Load tmpVar src
      return $ Var tmpVar

expectArr :: A.Ident -> A.P -> VarKind -> State Pass [Integer]
expectArr ident pos Scalar = do
  reportDiagn (Error pos $ printf "Variable %s has scalar kind, but an array was expected" ident)
  return []
expectArr _ _ (Array elements) = pure elements
expectArr _ _ BogusKind = pure []

expectScalar :: A.Ident -> A.P -> VarKind -> State Pass ()
expectScalar ident pos (Array _) =
  reportDiagn (Error pos $ printf "Variable %s has array kind, but a scalar was expected" ident)
expectScalar _ _ _ = pure ()
