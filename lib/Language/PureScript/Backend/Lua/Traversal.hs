{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.PureScript.Backend.Lua.Traversal where

import Language.PureScript.Backend.Lua.Types

everywhereExp
  :: (Exp -> Exp) -> (Statement -> Statement) -> Exp -> Exp
everywhereExp f g = runIdentity . everywhereExpM (pure . f) (pure . g)

everywhereStat
  :: (Statement -> Statement) -> (Exp -> Exp) -> Statement -> Statement
everywhereStat f g = runIdentity . everywhereStatM (pure . f) (pure . g)

everywhereExpM
  :: Monad m
  => (Exp -> m Exp)
  -> (Statement -> m Statement)
  -> Exp
  -> m Exp
everywhereExpM f g = goe
 where
  goe = \case
    Var v -> case v of
      VarIndex e1 e2 -> f =<< (Var .) . VarIndex <$> goe e1 <*> goe e2
      VarField e1 n -> f . Var . (`VarField` n) =<< goe e1
      VarName n -> f (Var (VarName n))
    Function names statements -> do
      args <- traverse (everywhereStatM g f) statements
      f $ Function names args
    TableCtor rows -> do
      tableRows <- forM rows \case
        TableRowKV k v -> TableRowKV <$> goe k <*> goe v
        TableRowNV n e -> TableRowNV n <$> goe e
      f $ TableCtor tableRows
    UnOp op e -> f . UnOp op =<< goe e
    BinOp op e1 e2 -> f =<< BinOp op <$> goe e1 <*> goe e2
    FunctionCall fn args -> f =<< FunctionCall <$> goe fn <*> traverse goe args
    other -> f other

everywhereStatM
  :: Monad m
  => (Statement -> m Statement)
  -> (Exp -> m Exp)
  -> Statement
  -> m Statement
everywhereStatM f g = go
 where
  goe = everywhereExpM g f
  go = \case
    Assign vars vals -> f . Assign vars =<< traverse goe vals
    Local names vals -> f . Local names =<< traverse goe vals
    IfThenElse p tb ef eb -> do
      predicate <- goe p
      thenBranch <- traverse go tb
      elseIf <- traverse (bitraverse goe (traverse go)) ef
      elseBranch <- traverse (traverse go) eb
      f $ IfThenElse predicate thenBranch elseIf elseBranch
    Return e -> f . Return =<< goe e
    ForeignSourceCode src -> f $ ForeignSourceCode src

--------------------------------------------------------------------------------
-- Indexing --------------------------------------------------------------------

data WithIndex a = WithIndex Natural a
  deriving stock (Eq, Show)

type IndexedStatement = Annotated WithIndex StatementF

indexStatement :: Statement -> State Natural IndexedStatement
indexStatement =
  annotateStatementM @(State Natural) @Identity @WithIndex identity \a ->
    state \i -> (WithIndex i a, i + 1)

unIndexStatement :: IndexedStatement -> Statement
unIndexStatement = unAnnotateStatement @WithIndex \(WithIndex _ a) -> a

--------------------------------------------------------------------------------
-- Annotating ------------------------------------------------------------------

unAnnotateStatement
  :: forall f
   . (forall g. Annotated f g -> g f)
  -> Annotated f StatementF
  -> Statement
unAnnotateStatement alg =
  runIdentity . annotateStatementM @Identity @f @Identity alg pure

annotateStatementM
  :: forall m f f'
   . Monad m
  => (forall g. Annotated f g -> g f)
  -> (forall x. x f' -> m (Annotated f' x))
  -> Annotated f StatementF
  -> m (Annotated f' StatementF)
annotateStatementM unAnnotate visit stat =
  case unAnnotate stat of
    Assign vars vals -> do
      indexedVars <- forM vars annotateV
      indexedVals <- forM vals annotateE
      visit $ Assign indexedVars indexedVals
    Local names vals -> visit . Local names =<< forM vals annotateE
    IfThenElse p tb ef eb -> do
      iPred <- annotateE p
      iThen <- traverse annotateS tb
      iElif <- traverse (bitraverse annotateE (traverse annotateS)) ef
      iElse <- traverse (traverse annotateS) eb
      visit $ IfThenElse iPred iThen iElif iElse
    Return e -> visit . Return =<< annotateE e
    ForeignSourceCode src -> visit $ ForeignSourceCode src
 where
  annotateS = annotateStatementM unAnnotate visit
  annotateE = annotateExpM unAnnotate visit
  annotateV = annotateVarM unAnnotate visit

annotateExpM
  :: forall m f f'
   . Monad m
  => (forall g. Annotated f g -> g f)
  -> (forall x. x f' -> m (Annotated f' x))
  -> Annotated f ExpF
  -> m (Annotated f' ExpF)
annotateExpM unAnnotate visit expf =
  case unAnnotate expf of
    Var v -> visit . Var =<< annotateV v
    Function names stats -> visit . Function names =<< forM stats annotateS
    TableCtor rows -> visit . TableCtor =<< forM rows annotateR
    UnOp op e -> visit . UnOp op =<< annotateE e
    BinOp op e1 e2 -> visit =<< BinOp op <$> annotateE e1 <*> annotateE e2
    FunctionCall fn args ->
      visit =<< FunctionCall <$> annotateE fn <*> forM args annotateE
    Nil -> visit Nil
    Boolean b -> visit $ Boolean b
    Integer i -> visit $ Integer i
    Float f -> visit $ Float f
    String s -> visit $ String s
 where
  annotateS = annotateStatementM unAnnotate visit
  annotateE = annotateExpM unAnnotate visit
  annotateV = annotateVarM unAnnotate visit
  annotateR = annotateTableRowM unAnnotate visit

annotateVarM
  :: forall m f f'
   . Monad m
  => (forall g. Annotated f g -> g f)
  -> (forall x. x f' -> m (Annotated f' x))
  -> Annotated f VarF
  -> m (Annotated f' VarF)
annotateVarM unAnnotate visit var =
  case unAnnotate var of
    VarName qualifiedName -> visit $ VarName qualifiedName
    VarIndex e1 e2 -> visit =<< VarIndex <$> annotateE e1 <*> annotateE e2
    VarField e name -> visit . (`VarField` name) =<< annotateE e
 where
  annotateE = annotateExpM unAnnotate visit

annotateTableRowM
  :: forall m f f'
   . Monad m
  => (forall g. Annotated f g -> g f)
  -> (forall x. x f' -> m (Annotated f' x))
  -> Annotated f TableRowF
  -> m (Annotated f' TableRowF)
annotateTableRowM unAnnotate visit row =
  case unAnnotate row of
    TableRowKV k v -> visit =<< TableRowKV <$> annotateE k <*> annotateE v
    TableRowNV n e -> visit . TableRowNV n =<< annotateE e
 where
  annotateE = annotateExpM unAnnotate visit
