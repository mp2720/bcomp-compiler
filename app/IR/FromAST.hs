{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module IR.FromAST (convert) where

import qualified AST
import Control.Lens.Basic (Lens)
import qualified Control.Lens.Basic as Lens
import Control.Monad (forM_, void)
import Control.Monad.Trans.State (State, execState, get, modify, put, runState)
import Diagnostics (Diagnostic (..), P)
import ID (FlatID, Symbols, bogusID, emptySymbols, newSymbol)
import IR
import IR.Scope (Scope)
import qualified IR.Scope as Scope
import Text.Printf (printf)

data Pass = Pass
  { -- | Reversed
    diagnostics :: [Diagnostic],
    labelsScope :: Scope (),
    varsScope :: Scope VarKind,
    vars :: Symbols VarKind,
    labels :: Symbols (),
    -- | Reversed
    instrs :: [LinearInstr]
  }

-- | Iff the program is malformed, diagnostics list with error is returned and linear program could
-- be bogus (so it should never be passed further along the compilation steps chain)
convert :: AST.Program -> ([Diagnostic], LinearProgram)
convert ast =
  ( reverse $ diagnostics pass,
    LinearProgram
      { progInstrs = reverse $ instrs pass,
        progVars = vars pass,
        progLabels = labels pass
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
            vars = emptySymbols,
            labels = emptySymbols,
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

type ScopeLens s = Lens Pass Pass (Scope s) (Scope s)

type SymsLens s = Lens Pass Pass (Symbols s) (Symbols s)

declSym ::
  ScopeLens s ->
  SymsLens s ->
  Maybe (P, AST.Ident) ->
  s ->
  State Pass FlatID
declSym scopeLens symsLens mbPosIdent s = do
  state <- get
  let scope = Lens.view scopeLens state
  let syms = Lens.view symsLens state

  -- Check for redeclaration if the symbol is from AST
  case mbPosIdent of
    Just (pos, ident) -> do
      case Scope.lookupSymbol ident scope of
        Just _ -> do
          reportDiagn $ Error pos (printf "symbol %s is redeclared" ident)
        Nothing -> return ()
    Nothing -> return ()

  let (flatID, updSyms) = newSymbol (snd <$> mbPosIdent) s syms
  -- Add to scope if the symbol is from AST
  let updScope = case mbPosIdent of
        Just (_, astID) -> Scope.addSymbol astID flatID s scope
        Nothing -> scope

  modify $ Lens.set symsLens updSyms
  modify $ Lens.set scopeLens updScope

  return flatID

declLabel ::
  Maybe (P, AST.Ident) ->
  State Pass Label
declLabel mbPosIdent =
  Label <$> declSym $(Lens.field 'labelsScope) $(Lens.field 'labels) mbPosIdent ()

declVar ::
  Maybe (P, AST.Ident) ->
  VarKind ->
  State Pass FlatID
declVar =
  declSym $(Lens.field 'varsScope) $(Lens.field 'vars)

resolveSym ::
  ScopeLens s ->
  s ->
  AST.Ident ->
  P ->
  State Pass (FlatID, s)
resolveSym scopeLens defaultS ident pos = do
  state <- get
  case Scope.lookupSymbolRec ident (Lens.view scopeLens state) of
    Nothing -> do
      reportDiagn $ Error pos (printf "unknown symbol %s" ident)
      return (bogusID, defaultS)
    Just entry -> return entry

resolveVar ::
  AST.Ident ->
  P ->
  State Pass (FlatID, VarKind)
resolveVar = resolveSym $(Lens.field 'varsScope) BogusKind

resolveLabel :: String -> P -> State Pass FlatID
resolveLabel ident pos = fst <$> resolveSym $(Lens.field 'labelsScope) () ident pos

-- Pass 1 (collect labels)

pass1 :: AST.Program -> State Pass ()
pass1 (AST.Program stms) = forM_ stms p1Stmt

p1Stmt :: AST.Stmt -> State Pass ()
p1Stmt (AST.ExecStmt _ stmt) = p1LabeledStmt stmt
p1Stmt _ = return ()

p1LabeledStmt :: AST.LabeledStmt -> State Pass ()
p1LabeledStmt (AST.LabeledStmt pos mbIdent execStmt) = do
  case mbIdent of
    Nothing -> pure ()
    Just ident -> void $ declLabel (Just (pos, ident))
  case execStmt of
    AST.Block _ stmts -> forM_ stmts p1Stmt
    AST.If _ _ then_ else_ -> do
      p1LabeledStmt then_
      p1LabeledStmt else_
    _ -> pure ()

-- Pass 2 (collect variables & emit IR)

pass2 :: AST.Program -> State Pass ()
pass2 (AST.Program stmts) = do
  start <- declLabel Nothing
  emitLabel start
  forM_ stmts p2Stmt

p2Stmt :: AST.Stmt -> State Pass ()
p2Stmt stmt = case stmt of
  (AST.ExecStmt _ labStmt) -> p2LabeledStmt labStmt
  --
  (AST.VarDef pos ident mbRexpr) -> do
    flatId <- declVar (Just (pos, ident)) Scalar
    forM_ mbRexpr (p2AssignToVar flatId)
  --
  (AST.ArrDef pos ident mbExplicitSize elements) -> do
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

    _ <- declVar (Just (pos, ident)) (Array elementsPadded)

    return ()

p2LabeledStmt :: AST.LabeledStmt -> State Pass ()
p2LabeledStmt (AST.LabeledStmt pos mbIdent stmt) = do
  case mbIdent of
    Nothing -> pure ()
    Just ident -> do
      label <- resolveLabel ident pos
      emitBranch $ Br $ Label label
      emitLabel $ Label label

  p2ExecStmt stmt

p2ExecStmt :: AST.ExecStmt -> State Pass ()
p2ExecStmt stmt = case stmt of
  (AST.Assign _ lhs rhs) -> p2Assign lhs rhs
  --
  (AST.If _ cond then_ else_) ->
    p2If cond (p2LabeledStmt then_) (p2LabeledStmt else_)
  --
  (AST.Goto pos label) -> do
    flatLabel <- resolveLabel label pos
    emitBranch $ Br $ Label flatLabel
    return ()
  --
  (AST.Block _ stmts) -> do
    state <- get
    let parentVarsScope = varsScope state
    let updState =
          execState
            (forM_ stmts p2Stmt)
            state
              { varsScope = Scope.mkChild $ varsScope state
              }
    put updState {varsScope = parentVarsScope}
  --
  (AST.NoOp _) -> return ()

p2Assign :: AST.LeftExpr -> AST.RightExpr -> State Pass ()
p2Assign left right = case left of
  (AST.PtrDeref _ dstExpr) -> do
    dst <- p2RightExpr dstExpr
    src <- p2RightExpr right
    emitSeq $ Store dst src
  (AST.Var varPos varId) -> do
    (varFlatId, varKind) <- resolveVar varId varPos
    expectScalar varId varPos varKind
    p2AssignToVar varFlatId right

-- | Assign to scalar var.
p2AssignToVar :: FlatID -> AST.RightExpr -> State Pass ()
p2AssignToVar varFlatId rexpr = do
  -- TODO: OPT: optimize redundant copy emitted for assign

  src <- p2RightExpr rexpr
  emitSeq $ Copy varFlatId src

p2If ::
  AST.RightExpr ->
  State Pass () ->
  State Pass () ->
  State Pass ()
p2If cond then_ else_ = do
  labelThen <- declLabel Nothing
  labelElse <- declLabel Nothing
  labelIfEnd <- declLabel Nothing

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
    emitCond (AST.RelOpApp _ leftExpr op rightExpr) labelThen labelElse = do
      l <- p2RightExpr leftExpr
      r <- p2RightExpr rightExpr
      emitBranch $
        ( case op of
            AST.Equals -> BrIf (IfEq l r)
            AST.NotEq -> flip $ BrIf (IfEq l r)
            AST.Gt -> flip $ BrIf (IfLt l r)
            AST.Geq -> BrIf (IfGe l r)
            AST.Lt -> BrIf (IfLt l r)
            AST.Leq -> flip $ BrIf (IfGe l r)
            AST.UnsignedGt -> flip $ BrIf (IfUnsignedLt l r)
            AST.UnsignedGeq -> BrIf (IfUnsignedGe l r)
            AST.UnsignedLt -> BrIf (IfUnsignedLt l r)
            AST.UnsignedLeq -> flip $ BrIf (IfUnsignedGe l r)
        )
          labelThen
          labelElse
    emitCond condExpr labelThen labelElse = do
      condOpnd <- p2RightExpr condExpr
      emitBranch $ BrIf (IfZero condOpnd) labelElse labelThen

p2RightExpr :: AST.RightExpr -> State Pass Operand
p2RightExpr rexpr = do
  -- TODO: OPT: optimize tmpVar out if not used

  tmpVarFlatId <- declVar Nothing Scalar
  p rexpr tmpVarFlatId
  where
    p (AST.Literal _ lit) _ =
      return $ Const lit
    --
    p (AST.BinOpApp _ leftExpr astOp rightExpr) tmpVar = do
      left <- p2RightExpr leftExpr
      right <- p2RightExpr rightExpr
      let irOp = case astOp of
            AST.Add -> Add
            AST.Sub -> Sub
            AST.BitAnd -> BitAnd
            AST.BitOr -> BitOr
      emitSeq $ BinOp tmpVar left irOp right
      return $ Var tmpVar
    --
    p condExpr@(AST.RelOpApp {}) tmpVar = do
      -- TODO: OPT: optimize code for case when logic expression is evaluated to var,
      -- then used in an if statemtn
      p2If
        condExpr
        (emitSeq $ Copy tmpVar (Const 1))
        (emitSeq $ Copy tmpVar (Const 0))
      return $ Var tmpVar
    --
    p (AST.UnaryOpApp _ astOp expr) tmpVar = do
      opnd <- p2RightExpr expr
      emitSeq $ case astOp of
        AST.BitNot -> BitNot tmpVar opnd
        AST.Negate -> Negate tmpVar opnd
      return $ Var tmpVar
    --
    p (AST.AddressOf _ (AST.Var varPos varId)) _ = do
      (varFlatId, _) <- resolveVar varId varPos
      return $ Address varFlatId
    --
    p (AST.AddressOf _ (AST.PtrDeref _ ptrExpr)) tmpVar =
      p ptrExpr tmpVar
    --
    p (AST.Sizeof pos arrId) _ = do
      (_, arrKind) <- resolveVar arrId pos
      elements <- expectArr arrId pos arrKind
      return $ Const $ fromIntegral (length elements)
    --
    p (AST.LeftExpr _ l) tmpVar =
      lexpr l tmpVar

    lexpr (AST.Var pos varId) _ = do
      (varFlatId, varKind) <- resolveVar varId pos
      return $ case varKind of
        Array _ -> Address varFlatId
        Scalar -> Var varFlatId
        BogusKind -> Var varFlatId
    --
    lexpr (AST.PtrDeref _ ptrExpr) tmpVar = do
      src <- p2RightExpr ptrExpr
      emitSeq $ Load tmpVar src
      return $ Var tmpVar

expectArr :: AST.Ident -> P -> VarKind -> State Pass [Integer]
expectArr ident pos Scalar = do
  reportDiagn (Error pos $ printf "Variable %s has scalar kind, but an array was expected" ident)
  return []
expectArr _ _ (Array elements) = pure elements
expectArr _ _ BogusKind = pure []

expectScalar :: AST.Ident -> P -> VarKind -> State Pass ()
expectScalar ident pos (Array _) =
  reportDiagn (Error pos $ printf "Variable %s has array kind, but a scalar was expected" ident)
expectScalar _ _ _ = pure ()
