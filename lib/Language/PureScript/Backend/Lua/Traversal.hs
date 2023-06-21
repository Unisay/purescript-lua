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
      f . functionDef (snd <$> names)
        =<< forM statements (everywhereStatM g f . unAnn)
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
  , annotateParam :: ParamF f' -> m (Annotated f' ParamF)
  -- ^ How to annotate a function parameter
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
        , annotateParam = pure . ann
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
    Function params stats -> do
      paramNames <- forM params \case
        (_, ParamNamed n) -> annotateParam (ParamNamed n)
        (_, ParamUnused) -> annotateParam ParamUnused
      aStats <- forM stats goS
      annotateExp $ Function paramNames aStats
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
  { aroundChunk :: [Annotated a StatementF] -> m [Annotated a StatementF]
  , beforeStat :: Annotated a StatementF -> m (Annotated a StatementF)
  , afterStat :: StatementF a -> m (StatementF a)
  , beforeExp :: Annotated a ExpF -> m (Annotated a ExpF)
  , afterExp :: ExpF a -> m (ExpF a)
  , beforeVar :: Annotated a VarF -> m (Annotated a VarF)
  , afterVar :: VarF a -> m (VarF a)
  , beforeRow :: Annotated a TableRowF -> m (Annotated a TableRowF)
  , afterRow :: TableRowF a -> m (TableRowF a)
  }

makeVisitor :: Applicative m => Visitor m a
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

visitStatementM
  :: Monad m
  => Visitor m a
  -> (Annotated a StatementF -> m (Annotated a StatementF))
visitStatementM visitor@Visitor {..} stat = do
  let goS = visitStatementM visitor
      goE = visitExpM visitor
      goV = visitVarM visitor
  beforeStat stat >>= traverse \case
    Assign variable value -> do
      indexedVars <- goV variable
      indexedVals <- goE value
      afterStat $ Assign indexedVars indexedVals
    Local names vals ->
      afterStat . Local names =<< forM vals goE
    IfThenElse p tb eb -> do
      iPred <- goE p
      iThen <- traverse goS tb
      iElse <- traverse goS eb
      afterStat $ IfThenElse iPred iThen iElse
    Return e -> afterStat . Return =<< goE e
    other -> afterStat other

visitExpM
  :: forall m a
   . Monad m
  => Visitor m a
  -> (Annotated a ExpF -> m (Annotated a ExpF))
visitExpM visitor@Visitor {..} expf = do
  let goS = visitStatementM visitor
      goE = visitExpM visitor
      goV = visitVarM visitor
  beforeExp expf >>= traverse \case
    Var v ->
      afterExp . Var =<< goV v
    Function names stats ->
      afterExp . Function names =<< forM stats goS
    TableCtor rows ->
      TableCtor <$> forM rows do
        beforeRow >=> traverse \case
          TableRowKV k v -> afterRow =<< TableRowKV <$> goE k <*> goE v
          TableRowNV n e -> afterRow . TableRowNV n =<< goE e
    UnOp op e ->
      afterExp . UnOp op =<< goE e
    BinOp op e1 e2 ->
      afterExp =<< BinOp op <$> goE e1 <*> goE e2
    FunctionCall fn args ->
      afterExp =<< FunctionCall <$> goE fn <*> forM args goE
    other -> afterExp other

visitVarM
  :: forall m a
   . Monad m
  => Visitor m a
  -> (Annotated a VarF -> m (Annotated a VarF))
visitVarM visitor@Visitor {..} variable = do
  let goE = visitExpM visitor
  beforeVar variable >>= traverse \case
    VarName qualifiedName -> afterVar $ VarName qualifiedName
    VarIndex e1 e2 -> afterVar =<< VarIndex <$> goE e1 <*> goE e2
    VarField e name -> afterVar . (`VarField` name) =<< goE e
