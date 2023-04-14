{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.PureScript.Backend.Lua.Traversal where

import Language.PureScript.Backend.Lua.Types
import Prelude hiding (local)

everywhereExp
  :: (Exp -> Exp) -> (Statement -> Statement) -> Exp -> Exp
everywhereExp f g = runIdentity . everywhereExpM (pure . f) (pure . g)

everywhereStat
  :: (Statement -> Statement) -> (Exp -> Exp) -> Statement -> Statement
everywhereStat f g = runIdentity . everywhereStatM (pure . f) (pure . g)

everywhereInChunkM
  :: Monad m
  => (Exp -> m Exp)
  -> (Statement -> m Statement)
  -> (Chunk -> m Chunk)
everywhereInChunkM f g = traverse (everywhereStatM g f)

everywhereExpM
  :: forall m
   . Monad m
  => (Exp -> m Exp)
  -> (Statement -> m Statement)
  -> (Exp -> m Exp)
everywhereExpM f g = goe
 where
  goe = \case
    Var (Ann v) -> case v of
      VarIndex (Ann e1) (Ann e2) -> f =<< varIndex <$> goe e1 <*> goe e2
      VarField (Ann e) n -> f . (`varField` n) =<< goe e
      VarName n -> f (varName n)
    Function names statements ->
      f . functionDef names =<< forM statements (everywhereStatM g f . unAnn)
    TableCtor (fmap unAnn -> rows) -> do
      tableRows <- forM rows \case
        TableRowKV (Ann k) (Ann v) -> tableRowKV <$> goe k <*> goe v
        TableRowNV n (Ann e) -> tableRowNV n <$> goe e
      f $ tableCtor tableRows
    UnOp op (Ann e) ->
      f . unOp op =<< goe e
    BinOp op (Ann e1) (Ann e2) ->
      f =<< binOp op <$> goe e1 <*> goe e2
    FunctionCall (Ann fn) (fmap unAnn -> args) ->
      f =<< functionCall <$> goe fn <*> forM args goe
    other -> f other

everywhereStatM
  :: forall m
   . Monad m
  => (Statement -> m Statement)
  -> (Exp -> m Exp)
  -> (Statement -> m Statement)
everywhereStatM f g = go
 where
  goe = everywhereExpM g f
  go = \case
    Assign (Ann variable) (Ann value) -> f . assign variable =<< goe value
    Local name val -> f . local name =<< forM val (goe . unAnn)
    IfThenElse (Ann p) tb eb -> do
      predicate <- goe p
      thenBranch <- forM tb (go . unAnn)
      elseBranch <- forM eb (go . unAnn)
      f $ ifThenElse predicate thenBranch elseBranch
    Return (Ann e) -> f . Return . ann =<< goe e
    ForeignSourceCode src -> f $ ForeignSourceCode src

--------------------------------------------------------------------------------
-- Annotating ------------------------------------------------------------------

data Annotator m f f' = Annotator
  { unAnnotate :: forall g. Annotated f g -> g f
  -- ^ How to remove an annotation
  , annotateStat :: StatementF f' -> m (Annotated f' StatementF)
  -- ^ How to annotate a statement
  , annotateExp :: ExpF f' -> m (Annotated f' ExpF)
  -- ^ How to annotate an expression
  , annotateVar :: VarF f' -> m (Annotated f' VarF)
  -- ^ How to annotate a variable
  , annotateRow :: TableRowF f' -> m (Annotated f' TableRowF)
  -- ^ How to annotate a table row
  }

unAnnotateStatement
  :: (forall g. Annotated f g -> g f) -> Annotated f StatementF -> Statement
unAnnotateStatement unAnnotate =
  unAnn
    . runIdentity
    . annotateStatementInsideOutM
      Annotator
        { unAnnotate
        , annotateStat = pure . ann
        , annotateExp = pure . ann
        , annotateVar = pure . ann
        , annotateRow = pure . ann
        }

--------------------------------------------------------------------------------
-- Inside-out ------------------------------------------------------------------

annotateStatementInsideOutM
  :: forall m f f'
   . Monad m
  => Annotator m f f'
  -> (Annotated f StatementF -> m (Annotated f' StatementF))
annotateStatementInsideOutM annotator@Annotator {..} stat =
  case unAnnotate stat of
    Assign variable value -> do
      indexedVars <- goV variable
      indexedVals <- goE value
      annotateStat $ Assign indexedVars indexedVals
    Local names vals -> annotateStat . Local names =<< forM vals goE
    IfThenElse p tb eb -> do
      iPred <- goE p
      iThen <- traverse goS tb
      iElse <- traverse goS eb
      annotateStat $ IfThenElse iPred iThen iElse
    Return e -> annotateStat . Return =<< goE e
    ForeignSourceCode src -> annotateStat $ ForeignSourceCode src
 where
  goS = annotateStatementInsideOutM annotator
  goE = annotateExpInsideOutM annotator
  goV = annotateVarInsideOutM annotator

annotateExpInsideOutM
  :: forall m f f'
   . Monad m
  => Annotator m f f'
  -> (Annotated f ExpF -> m (Annotated f' ExpF))
annotateExpInsideOutM annotator@Annotator {..} expf =
  case unAnnotate expf of
    Var v -> annotateExp . Var =<< goV v
    Function names stats -> annotateExp . Function names =<< forM stats goS
    TableCtor rows ->
      annotateExp . TableCtor =<< forM rows \row ->
        case unAnnotate row of
          TableRowKV k v -> annotateRow =<< TableRowKV <$> goE k <*> goE v
          TableRowNV n e -> annotateRow . TableRowNV n =<< goE e
    UnOp op e -> annotateExp . UnOp op =<< goE e
    BinOp op e1 e2 -> annotateExp =<< BinOp op <$> goE e1 <*> goE e2
    FunctionCall fn args ->
      annotateExp =<< FunctionCall <$> goE fn <*> forM args goE
    Nil -> annotateExp Nil
    Boolean b -> annotateExp $ Boolean b
    Integer i -> annotateExp $ Integer i
    Float f -> annotateExp $ Float f
    String s -> annotateExp $ String s
 where
  goS = annotateStatementInsideOutM annotator
  goE = annotateExpInsideOutM annotator
  goV = annotateVarInsideOutM annotator

annotateVarInsideOutM
  :: forall m f f'
   . Monad m
  => Annotator m f f'
  -> (Annotated f VarF -> m (Annotated f' VarF))
annotateVarInsideOutM annotator@Annotator {..} =
  unAnnotate >>> \case
    VarName qualifiedName -> annotateVar $ VarName qualifiedName
    VarIndex e1 e2 -> annotateVar =<< VarIndex <$> goE e1 <*> goE e2
    VarField e name -> annotateVar . (`VarField` name) =<< goE e
 where
  goE = annotateExpInsideOutM annotator

--------------------------------------------------------------------------------
-- Outside-in ------------------------------------------------------------------

data Visitor m a = Visitor
  { visitStat :: Annotated a StatementF -> m (Annotated a StatementF)
  , visitExp :: Annotated a ExpF -> m (Annotated a ExpF)
  , visitVar :: Annotated a VarF -> m (Annotated a VarF)
  , visitRow :: Annotated a TableRowF -> m (Annotated a TableRowF)
  }

noopVisitor :: Applicative m => Visitor m a
noopVisitor =
  Visitor {visitStat = pure, visitExp = pure, visitVar = pure, visitRow = pure}

visitStatementOutsideInM
  :: forall m a
   . Monad m
  => Visitor m a
  -> Annotated a StatementF
  -> m (Annotated a StatementF)
visitStatementOutsideInM visitor@Visitor {..} stat = do
  let goS = visitStatementOutsideInM visitor
      goE = visitExpOutsideInM visitor
      goV = visitVarOutsideInM visitor
  visitStat stat >>= traverse \case
    Assign variable value -> do
      indexedVars <- goV variable
      indexedVals <- goE value
      pure $ Assign indexedVars indexedVals
    Local names vals -> Local names <$> forM vals goE
    IfThenElse p tb eb -> do
      iPred <- goE p
      iThen <- traverse goS tb
      iElse <- traverse goS eb
      pure $ IfThenElse iPred iThen iElse
    Return e -> Return <$> goE e
    other -> pure other

visitExpOutsideInM
  :: forall m a
   . Monad m
  => Visitor m a
  -> (Annotated a ExpF -> m (Annotated a ExpF))
visitExpOutsideInM visitor@Visitor {..} expf = do
  let goS = visitStatementOutsideInM visitor
      goE = visitExpOutsideInM visitor
      goV = visitVarOutsideInM visitor
  visitExp expf >>= \expr -> forM expr \case
    Var v -> Var <$> goV v
    Function names stats -> Function names <$> forM stats goS
    TableCtor rows ->
      TableCtor <$> forM rows do
        visitRow >=> traverse \case
          TableRowKV k v -> TableRowKV <$> goE k <*> goE v
          TableRowNV n e -> TableRowNV n <$> goE e
    UnOp op e -> UnOp op <$> goE e
    BinOp op e1 e2 -> BinOp op <$> goE e1 <*> goE e2
    FunctionCall fn args -> FunctionCall <$> goE fn <*> forM args goE
    _ -> unAnn <$> visitExp expr

visitVarOutsideInM
  :: forall m a
   . Monad m
  => Visitor m a
  -> (Annotated a VarF -> m (Annotated a VarF))
visitVarOutsideInM visitor@Visitor {..} variable = do
  let goE = visitExpOutsideInM visitor
  visitVar variable >>= traverse \case
    VarName qualifiedName -> pure $ VarName qualifiedName
    VarIndex e1 e2 -> VarIndex <$> goE e1 <*> goE e2
    VarField e name -> (`VarField` name) <$> goE e
