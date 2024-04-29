{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.PureScript.Backend.Lua.Traversal where

import Language.PureScript.Backend.Lua.Types
import Prelude hiding (local)

everywhereExp
  ∷ (Exp → Exp) → (Statement → Statement) → Exp → Exp
everywhereExp f g = runIdentity . everywhereExpM (pure . f) (pure . g)

everywhereStat
  ∷ (Statement → Statement) → (Exp → Exp) → Statement → Statement
everywhereStat f g = runIdentity . everywhereStatM (pure . f) (pure . g)

everywhereInChunkM
  ∷ Monad m
  ⇒ (Exp → m Exp)
  → (Statement → m Statement)
  → (Chunk → m Chunk)
everywhereInChunkM f g = traverse (everywhereStatM g f)

everywhereExpM
  ∷ ∀ m
   . Monad m
  ⇒ (Exp → m Exp)
  → (Statement → m Statement)
  → (Exp → m Exp)
everywhereExpM f g = goe
 where
  goe = \case
    Var _ann v → case v of
      VarIndex _ann e1 e2 → f =<< varIndex <$> goe e1 <*> goe e2
      VarField _ann e n → f . (`varField` n) =<< goe e
      VarName _ann n → f (varName n)
    Function _ann names statements →
      f . functionDef names
        =<< forM statements (everywhereStatM g f)
    TableCtor _ann rows → do
      tableRows ← forM rows \case
        TableRowKV _ann k v → tableRowKV <$> goe k <*> goe v
        TableRowNV _ann n e → tableRowNV n <$> goe e
      f $ table tableRows
    UnOp _ann op e →
      f . unOp op =<< goe e
    BinOp _ann op e1 e2 →
      f =<< binOp op <$> goe e1 <*> goe e2
    FunctionCall _ann fn args →
      f =<< functionCall <$> goe fn <*> forM args goe
    other → f other

everywhereStatM
  ∷ ∀ m
   . Monad m
  ⇒ (Statement → m Statement)
  → (Exp → m Exp)
  → (Statement → m Statement)
everywhereStatM f g = go
 where
  goe = everywhereExpM g f
  go = \case
    Assign ann variable value → f . Assign ann variable =<< goe value
    Local ann name val → f . Local ann name =<< forM val goe
    IfThenElse ann p tb eb → do
      predicate ← goe p
      thenBranch ← forM tb go
      elseBranch ← forM eb go
      f $ IfThenElse ann predicate thenBranch elseBranch
    Return ann e → f . Return ann =<< goe e
    ForeignSourceStat ann src → f $ ForeignSourceStat ann src

--------------------------------------------------------------------------------
-- Annotating ------------------------------------------------------------------

data Annotator m f f' = Annotator
  { withAnn ∷ f → m f'
  -- ^ How to update the annotation
  , annotateStat ∷ StatementF f' → m (StatementF f')
  -- ^ How to annotate a statement
  , annotateExp ∷ ExpF f' → m (ExpF f')
  -- ^ How to annotate an expression
  , annotateParam ∷ ParamF f' → m (ParamF f')
  -- ^ How to annotate a function parameter
  , annotateVar ∷ VarF f' → m (VarF f')
  -- ^ How to annotate a variable
  , annotateRow ∷ TableRowF f' → m (TableRowF f')
  -- ^ How to annotate a table row
  }

--------------------------------------------------------------------------------
-- Inside-out ------------------------------------------------------------------

annotateStatementInsideOutM
  ∷ ∀ m f f'. Monad m ⇒ Annotator m f f' → StatementF f → m (StatementF f')
annotateStatementInsideOutM annotator@Annotator {..} = \case
  Assign ann variable value → do
    visitedVars ← goV variable
    visitedVals ← goE value
    ann' ← withAnn ann
    annotateStat $ Assign ann' visitedVars visitedVals
  Local ann names vals → do
    ann' ← withAnn ann
    annotateStat . Local ann' names =<< forM vals goE
  IfThenElse ann p tb eb → do
    ann' ← withAnn ann
    iPred ← goE p
    iThen ← traverse goS tb
    iElse ← traverse goS eb
    annotateStat $ IfThenElse ann' iPred iThen iElse
  Return ann e → do
    ann' ← withAnn ann
    e' ← goE e
    annotateStat $ Return ann' e'
  ForeignSourceStat ann src → do
    ann' ← withAnn ann
    annotateStat $ ForeignSourceStat ann' src
 where
  goS = annotateStatementInsideOutM annotator
  goE = annotateExpInsideOutM annotator
  goV = annotateVarInsideOutM annotator

annotateExpInsideOutM
  ∷ ∀ m f f'. Monad m ⇒ Annotator m f f' → (ExpF f → m (ExpF f'))
annotateExpInsideOutM annotator@Annotator {..} = \case
  Var ann v → do
    ann' ← withAnn ann
    v' ← goV v
    annotateExp $ Var ann' v'
  Function ann params stats → do
    paramNames ← forM params \case
      ParamNamed pann n → do
        pann' ← withAnn pann
        annotateParam (ParamNamed pann' n)
      ParamUnused pann → do
        pann' ← withAnn pann
        annotateParam (ParamUnused pann')
    ann' ← withAnn ann
    stats' ← forM stats goS
    annotateExp $ Function ann' paramNames stats'
  TableCtor ann rows → do
    ann' ← withAnn ann
    rows' ← forM rows \case
      TableRowKV tann k v → do
        tann' ← withAnn tann
        k' ← goE k
        v' ← goE v
        annotateRow $ TableRowKV tann' k' v'
      TableRowNV tann n e → do
        tann' ← withAnn tann
        e' ← goE e
        annotateRow $ TableRowNV tann' n e'
    annotateExp $ TableCtor ann' rows'
  UnOp ann op e → do
    ann' ← withAnn ann
    e' ← goE e
    annotateExp $ UnOp ann' op e'
  BinOp ann op e1 e2 → do
    ann' ← withAnn ann
    e1' ← goE e1
    e2' ← goE e2
    annotateExp $ BinOp ann' op e1' e2'
  FunctionCall ann fn args → do
    ann' ← withAnn ann
    fn' ← goE fn
    args' ← forM args goE
    annotateExp $ FunctionCall ann' fn' args'
  Nil ann → do
    ann' ← withAnn ann
    annotateExp (Nil ann')
  Boolean ann b → do
    ann' ← withAnn ann
    annotateExp $ Boolean ann' b
  Integer ann i → do
    ann' ← withAnn ann
    annotateExp $ Integer ann' i
  Float ann f → do
    ann' ← withAnn ann
    annotateExp $ Float ann' f
  String ann s → do
    ann' ← withAnn ann
    annotateExp $ String ann' s
  ForeignSourceExp ann src → do
    ann' ← withAnn ann
    annotateExp $ ForeignSourceExp ann' src
 where
  goS = annotateStatementInsideOutM annotator
  goE = annotateExpInsideOutM annotator
  goV = annotateVarInsideOutM annotator

annotateVarInsideOutM
  ∷ ∀ m f f'. Monad m ⇒ Annotator m f f' → (VarF f → m (VarF f'))
annotateVarInsideOutM annotator@Annotator {..} = \case
  VarName ann qualifiedName → do
    ann' ← withAnn ann
    annotateVar $ VarName ann' qualifiedName
  VarIndex ann e1 e2 → do
    ann' ← withAnn ann
    e1' ← goE e1
    e2' ← goE e2
    annotateVar $ VarIndex ann' e1' e2'
  VarField ann e name → do
    ann' ← withAnn ann
    e' ← goE e
    annotateVar $ VarField ann' e' name
 where
  goE = annotateExpInsideOutM annotator

--------------------------------------------------------------------------------
-- Outside-in ------------------------------------------------------------------

data Visitor m a = Visitor
  { aroundChunk ∷ [StatementF a] → m [StatementF a]
  , beforeStat ∷ StatementF a → m (StatementF a)
  , afterStat ∷ StatementF a → m (StatementF a)
  , beforeExp ∷ ExpF a → m (ExpF a)
  , afterExp ∷ ExpF a → m (ExpF a)
  , beforeVar ∷ VarF a → m (VarF a)
  , afterVar ∷ VarF a → m (VarF a)
  , beforeRow ∷ TableRowF a → m (TableRowF a)
  , afterRow ∷ TableRowF a → m (TableRowF a)
  }

makeVisitor ∷ Applicative m ⇒ Visitor m a
makeVisitor =
  Visitor
    { aroundChunk = pure
    , beforeStat = pure
    , afterStat = pure
    , beforeExp = pure
    , afterExp = pure
    , beforeVar = pure
    , afterVar = pure
    , beforeRow = pure
    , afterRow = pure
    }

visitStatementM ∷ Monad m ⇒ Visitor m a → (StatementF a → m (StatementF a))
visitStatementM visitor@Visitor {..} stat =
  beforeStat stat >>= \case
    Assign ann variable value → do
      visitedVars ← visitVarM visitor variable
      visitedVals ← visitExpM visitor value
      afterStat $ Assign ann visitedVars visitedVals
    Local ann names vals →
      afterStat . Local ann names =<< forM vals (visitExpM visitor)
    IfThenElse ann p tb eb → do
      iPred ← visitExpM visitor p
      iThen ← traverse (visitStatementM visitor) tb
      iElse ← traverse (visitStatementM visitor) eb
      afterStat $ IfThenElse ann iPred iThen iElse
    Return ann e →
      afterStat . Return ann =<< visitExpM visitor e
    other →
      afterStat other

visitExpM ∷ ∀ m a. Monad m ⇒ Visitor m a → (ExpF a → m (ExpF a))
visitExpM visitor@Visitor {..} expf = do
  beforeExp expf >>= \case
    Var ann v →
      afterExp . Var ann =<< visitVarM visitor v
    Function ann names stats →
      afterExp . Function ann names =<< forM stats (visitStatementM visitor)
    TableCtor ann rows →
      TableCtor ann <$> forM rows do
        beforeRow >=> \case
          TableRowKV ann' k v →
            afterRow
              =<< TableRowKV ann'
                <$> visitExpM visitor k
                <*> visitExpM visitor v
          TableRowNV ann' n e →
            afterRow . TableRowNV ann' n =<< visitExpM visitor e
    UnOp ann op e →
      afterExp . UnOp ann op =<< visitExpM visitor e
    BinOp ann op e1 e2 →
      afterExp
        =<< BinOp ann op
          <$> visitExpM visitor e1
          <*> visitExpM visitor e2
    FunctionCall ann fn args →
      afterExp
        =<< FunctionCall ann
          <$> visitExpM visitor fn
          <*> forM args (visitExpM visitor)
    other → afterExp other

visitVarM ∷ ∀ m a. Monad m ⇒ Visitor m a → (VarF a → m (VarF a))
visitVarM visitor@Visitor {..} variable =
  beforeVar variable >>= \case
    VarName ann qualifiedName →
      afterVar $ VarName ann qualifiedName
    VarIndex ann e1 e2 →
      afterVar
        =<< VarIndex ann
          <$> visitExpM visitor e1
          <*> visitExpM visitor e2
    VarField ann e name →
      afterVar . (\x → VarField ann x name) =<< visitExpM visitor e
