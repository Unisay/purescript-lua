module Language.PureScript.Backend.Lua
  ( module Lua
  , fromIrModule
  , fromModuleName
  , fromName
  , Error (..)
  ) where

import Control.Monad (ap)
import Control.Monad.Error.Class (MonadError (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Data.Traversable (for)
import Language.PureScript.Backend.IR.Types qualified as IR
import Language.PureScript.Backend.Lua.Linker qualified as Linker
import Language.PureScript.Backend.Lua.Name (Name)
import Language.PureScript.Backend.Lua.Name qualified as Name
import Language.PureScript.Backend.Lua.Optimizer (optimizeChunk)
import Language.PureScript.Backend.Lua.Types as Lua
import Prelude hiding (exp, local)

type LuaM a = StateT Natural (Either Error) a

data Error = UnexpectedRefBound IR.Exp IR.Index
  deriving stock (Show)

fromIrModule :: IR.Module -> Either Error Lua.Module
fromIrModule irModule = (`evalStateT` 0) do
  chunk <- do
    join <$> forM (IR.moduleBindings irModule) \case
      IR.Standalone (name, irExp) -> do
        exp <- fromExp moduleName irExp
        pure [local1 (fromName name) exp]
      IR.RecursiveGroup binds ->
        recBindStatements <$> forM binds \(name, irExp) ->
          (fromName name,) <$> fromExp moduleName irExp
  pure
    Lua.Module
      { moduleChunk = optimizeChunk chunk
      , moduleName
      , moduleImports
      , moduleExports
      , moduleForeigns
      , modulePath
      }
 where
  moduleImports = fromModuleName <$> IR.moduleImports irModule
  moduleExports = fromName <$> toList (IR.moduleExports irModule)
  moduleForeigns = fromName <$> IR.moduleForeigns irModule
  moduleName = fromModuleName (IR.moduleName irModule)
  modulePath = IR.modulePath irModule

fromName :: HasCallStack => IR.Name -> Name
fromName = Name.makeSafe . IR.nameToText

fromPropName :: IR.PropName -> Name
fromPropName (IR.PropName name) = Name.makeSafe name

fromModuleName :: IR.ModuleName -> ModuleName
fromModuleName = ModuleName . Name.makeSafe . IR.renderModuleName

fromExp :: ModuleName -> IR.Exp -> LuaM Exp
fromExp modname origExp = go origExp
 where
  go :: IR.Exp -> LuaM Exp =
    IR.unExp >>> \case
      IR.Lit literal ->
        case literal of
          IR.Integer i ->
            pure $ Integer i
          IR.Floating d ->
            pure $ Float d
          IR.String s ->
            pure $ String s
          IR.Char c ->
            pure $ String $ Text.singleton c
          IR.Boolean b ->
            pure $ Boolean b
          IR.Array exprs ->
            table <$> traverse (fmap TableRowV . go) exprs
          IR.Object kvs ->
            table <$> for kvs \(prop, exp) ->
              TableRowNV (fromPropName prop) <$> go exp
      IR.Prim op ->
        case op of
          IR.ArrayLength e ->
            hash <$> go e
          IR.ReflectCtor e ->
            flip varIndex keyCtor <$> go e
          IR.DataArgumentByIndex i e ->
            flip varIndex (Integer (fromIntegral i)) <$> go e
          IR.Eq l r ->
            equalTo <$> go l <*> go r
      IR.Ctor algebraicTy tyName ctorName fieldNames ->
        pure $ fromCtor modname algebraicTy tyName ctorName fieldNames
      IR.ArrayIndex expr index ->
        flip varIndex (Integer (fromIntegral index)) <$> go expr
      IR.ObjectProp expr propName ->
        flip varField (fromPropName propName) <$> go expr
      IR.ObjectUpdate _expr _patches ->
        Prelude.error "fromObjectUpdate is not implemented"
      IR.Abs binding -> do
        (name, expr) <- IR.unbindAbs binding
        go expr <&> \e ->
          Function (maybe [] (pure . fromName) name) [Lua.Return e]
      IR.App expr arg -> do
        e <- go expr
        a <- go arg
        pure $ Lua.functionCall e [a]
      IR.RefFree qualifiedName ->
        pure case qualifiedName of
          IR.Local name -> varName (fromName name)
          IR.Imported modname' name ->
            Linker.linkedVar (fromModuleName modname') (fromName name)
      IR.RefBound index -> throwError $ UnexpectedRefBound origExp index
      IR.Let binding -> do
        (bindings, bodyExp) <- IR.unbindLet binding
        body <- go bodyExp
        recs <-
          join <$> for (toList bindings) \case
            IR.Standalone (name, expr) -> do
              e <- go expr
              pure [local1 (fromName name) e]
            IR.RecursiveGroup grp -> do
              let binds =
                    toList grp <&> \(name, _) -> Local (pure (fromName name)) []
              assignments <- forM (toList grp) \(name, expr) ->
                assign1 (VarName (LocalName (fromName name))) <$> go expr
              pure $ binds <> assignments
        pure . scope $ join [recs, [Lua.Return body]]
      IR.IfThenElse cond th el ->
        fromIfThenElse <$> go cond <*> go th <*> go el
      IR.Exception msg ->
        pure $ Lua.error msg

keyCtor :: Exp
keyCtor = String "$ctor"

fromCtor
  :: ModuleName
  -> IR.AlgebraicType
  -> IR.TyName
  -> IR.CtorName
  -> [IR.FieldName]
  -> Exp
fromCtor currentModule _algTy _tyName ctorName fields =
  Lua.Function args [Lua.Return value]
 where
  value = table $ ctorRow : attributes
  ctorValue =
    Name.toText (unModuleName currentModule)
      <> "."
      <> IR.renderCtorName ctorName
  ctorRow = TableRowKV keyCtor (String ctorValue)
  args = Name.unsafeName . IR.renderFieldName <$> fields
  attributes = args <&> ap TableRowNV varName

recBindStatements :: NonEmpty (Name, Exp) -> [Statement]
recBindStatements binds = Local names [] : (uncurry assign1 <$> assigns)
 where
  names :: NonEmpty Name = fst <$> binds
  assigns :: [(Var, Exp)] = first (VarName . LocalName) <$> toList binds

fromIfThenElse :: Exp -> Exp -> Exp -> Exp
fromIfThenElse cond thenExp elseExp = Lua.functionCall fun []
 where
  thenBranch = NE.singleton (Lua.Return thenExp)
  elseBranch = NE.singleton (Lua.Return elseExp)
  fun = Lua.Function [] [Lua.IfThenElse cond thenBranch [] (Just elseBranch)]
