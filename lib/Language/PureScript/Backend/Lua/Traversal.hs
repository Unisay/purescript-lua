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
        TableRowV e -> TableRowV <$> goe e
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
