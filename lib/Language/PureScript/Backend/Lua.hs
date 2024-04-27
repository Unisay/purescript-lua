module Language.PureScript.Backend.Lua
  ( fromUberModule
  , fromIR
  , fromName
  , qualifyName
  , Error (..)
  ) where

import Control.Arrow (left)
import Control.Monad (ap)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Control.Monad.Trans.Accum (AccumT, add, runAccumT)
import Data.DList qualified as DList
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Tagged (Tagged (..), untag)
import Data.Text qualified as Text
import Data.Traversable (for)
import Language.PureScript.Backend.IR qualified as IR
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Linker qualified as Linker
import Language.PureScript.Backend.IR.Query (usesRuntimeLazy)
import Language.PureScript.Backend.Lua.Fixture qualified as Fixture
import Language.PureScript.Backend.Lua.Key qualified as Key
import Language.PureScript.Backend.Lua.Linker.Foreign qualified as Foreign
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Name qualified as Name
import Language.PureScript.Backend.Lua.Types (ParamF (..))
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Language.PureScript.Backend.AppOrModule (AppOrModule (..))
import Language.PureScript.Names (ModuleName (..), runModuleName)
import Language.PureScript.Names qualified as PS
import Path (Abs, Dir, Path)
import Prelude hiding (exp, local)

type LuaM e a =
  AccumT UsesObjectUpdate (StateT Natural (ExceptT (Variant e) IO)) a

data UsesObjectUpdate = NoObjectUpdate | UsesObjectUpdate
  deriving stock (Eq, Ord, Show)

instance Semigroup UsesObjectUpdate where
  _ <> UsesObjectUpdate = UsesObjectUpdate
  UsesObjectUpdate <> _ = UsesObjectUpdate
  NoObjectUpdate <> NoObjectUpdate = NoObjectUpdate

instance Monoid UsesObjectUpdate where
  mempty = NoObjectUpdate

data Error
  = UnexpectedRefBound ModuleName IR.Exp
  | LinkerErrorForeign Foreign.Error
  | AppEntryPointNotFound ModuleName PS.Ident
  deriving stock (Show)

fromUberModule
  ∷ ∀ e
   . e `CouldBe` Error
  ⇒ Tagged "foreign" (Path Abs Dir)
  → Tagged "needsRuntimeLazy" Bool
  → AppOrModule
  → Linker.UberModule
  → ExceptT (Variant e) IO Lua.Chunk
fromUberModule foreigns needsRuntimeLazy appOrModule uber = (`evalStateT` 0) do
  ((bindings, returnStat), usesObjectUpdate) ← (`runAccumT` NoObjectUpdate) do
    foreignBindings ←
      forM (Linker.uberModuleForeigns uber) \(IR.QName modname name, irExp) → do
        exp ← asExpression <$> fromIR foreigns Set.empty modname irExp
        pure $ mkBinding modname (fromName name) exp

    bindings ←
      Linker.uberModuleBindings uber & foldMapM \case
        IR.Standalone (IR.QName modname name, irExp) → do
          exp ← fromIR foreigns Set.empty modname irExp
          pure . DList.singleton $
            mkBinding modname (fromName name) (asExpression exp)
        IR.RecursiveGroup recGroup → do
          recBinds ← forM (toList recGroup) \(IR.QName modname name, irExp) →
            (modname,name,) . asExpression
              <$> fromIR foreigns Set.empty modname irExp
          pure $ DList.fromList do
            (modname, name, exp) ← recBinds
            pure $ mkBinding modname (fromName name) exp

    returnExp ←
      case appOrModule of
        AsModule modname →
          Lua.table <$> forM (uberModuleExports uber) \(fromName → name, expr) →
            Lua.tableRowNV name . asExpression
              <$> fromIR foreigns mempty modname expr
        AsApplication modname ident → do
          case List.lookup name (uberModuleExports uber) of
            Just expr → do
              entry ← fromIR foreigns mempty modname expr
              pure $ Lua.functionCall (asExpression entry) []
            _ → Oops.throw $ AppEntryPointNotFound modname ident
         where
          name = IR.identToName ident

    pure
      ( DList.fromList foreignBindings <> bindings
      , Lua.Return (Lua.ann returnExp)
      )

  pure $
    DList.fromList
      [ Fixture.runtimeLazy
      | untag needsRuntimeLazy && usesRuntimeLazy uber
      ]
      <> DList.fromList
        [ Fixture.objectUpdate
        | UsesObjectUpdate ← [usesObjectUpdate]
        ]
      <> DList.fromList
        [ Lua.local1 Fixture.moduleName (Lua.table [])
        | not (null bindings)
        ]
      <> DList.snoc bindings returnStat

mkBinding ∷ ModuleName → Lua.Name → Lua.Exp → Lua.Statement
mkBinding modname name =
  Lua.assign $
    Lua.VarField
      (Lua.ann (Lua.varName Fixture.moduleName))
      (qualifyName modname name)

asExpression ∷ Either Lua.Chunk Lua.Exp → Lua.Exp
asExpression = \case
  Left chunk → Lua.chunkToExpression chunk
  Right expr → expr

fromName ∷ HasCallStack ⇒ IR.Name → Lua.Name
fromName = Name.makeSafe . IR.nameToText

fromNameWithIndex ∷ HasCallStack ⇒ IR.Name → IR.Index → Lua.Name
fromNameWithIndex name (IR.unIndex → index) =
  if index == 0
    then fromName name
    else Name.makeSafe $ IR.nameToText name <> show index

fromModuleName ∷ ModuleName → Lua.Name
fromModuleName = Name.makeSafe . runModuleName

fromPropName ∷ IR.PropName → Lua.Name
fromPropName (IR.PropName name) = Name.makeSafe name

fromIR
  ∷ ∀ e
   . e `CouldBe` Error
  ⇒ Tagged "foreign" (Path Abs Dir)
  → Set Lua.Name
  → ModuleName
  → IR.Exp
  → LuaM e (Either Lua.Chunk Lua.Exp)
fromIR foreigns topLevelNames modname ir = case ir of
  IR.LiteralInt _ann i →
    pure . Right $ Lua.Integer i
  IR.LiteralFloat _ann d →
    pure . Right $ Lua.Float d
  IR.LiteralString _ann s →
    pure . Right $ Lua.String s
  IR.LiteralChar _ann c →
    pure . Right $ Lua.String $ Text.singleton c
  IR.LiteralBool _ann b →
    pure . Right $ Lua.Boolean b
  IR.LiteralArray _ann exprs →
    Right . Lua.table <$> forM (zip [1 ..] exprs) \(i, e) →
      Lua.tableRowKV (Lua.Integer i) <$> goExp e
  IR.LiteralObject _ann kvs →
    Right . Lua.table <$> for kvs \(prop, exp) →
      Lua.tableRowNV (fromPropName prop) <$> goExp exp
  IR.ReflectCtor _ann e →
    Right . (`Lua.varIndex` keyCtor) <$> goExp e
  IR.DataArgumentByIndex _ann i e →
    Right . (`Lua.varField` Lua.unsafeName ("value" <> show i)) <$> goExp e
  IR.Eq _ann l r →
    Right <$> liftA2 Lua.equalTo (goExp l) (goExp r)
  IR.Ctor _ann _algebraicTy ctorModName ctorTyName ctorName fieldNames →
    pure . Right $ foldr wrap value args
   where
    wrap name expr = Lua.functionDef [ParamNamed name] [Lua.return expr]
    value = Lua.table $ ctorRow : attributes
    ctorId = IR.ctorId ctorModName ctorTyName ctorName
    ctorRow = Lua.tableRowKV keyCtor (Lua.String ctorId)
    args = Name.unsafeName . IR.renderFieldName <$> fieldNames
    attributes = args <&> ap Lua.tableRowNV Lua.varName
  IR.ArrayLength _ann e →
    Right . Lua.hash <$> goExp e
  IR.ArrayIndex _ann expr index →
    Right . flip Lua.varIndex (Lua.Integer (fromIntegral index)) <$> goExp expr
  IR.ObjectProp _ann expr propName →
    Right . flip Lua.varField (fromPropName propName) <$> goExp expr
  IR.ObjectUpdate _ann expr propValues → do
    add UsesObjectUpdate
    obj ← goExp expr
    vals ←
      Lua.table <$> for (toList propValues) \(propName, e) →
        Lua.tableRowNV (fromPropName propName) <$> goExp e
    pure . Right $
      Lua.functionCall (Lua.varName Fixture.objectUpdateName) [obj, vals]
  IR.Abs _ann param expr → do
    e ← goExp expr
    let luaParams = case param of
          IR.ParamUnused _ann → []
          IR.ParamNamed _ann name → [ParamNamed (fromName name)]
    pure . Right $ Lua.functionDef luaParams [Lua.return e]
  IR.App _ann expr arg → do
    e ← goExp expr
    Right . Lua.functionCall e <$> case arg of
      -- PS sometimes inserts syntetic unused argument "Prim.undefined"
      IR.Ref _ann (IR.Imported (IR.ModuleName "Prim") (IR.Name "undefined")) _ →
        pure []
      _ → goExp arg <&> (: [])
  IR.Ref _ann qualifiedName index →
    pure . Right $ case qualifiedName of
      IR.Local name
        | topLevelName ← qualifyName modname (fromName name)
        , Set.member topLevelName topLevelNames →
            Lua.varField (Lua.varName Fixture.moduleName) topLevelName
      IR.Local name →
        Lua.varName (fromNameWithIndex name index)
      IR.Imported modname' name →
        Lua.varField
          (Lua.varName Fixture.moduleName)
          (qualifyName modname' (fromName name))
  IR.Let _ann bindings bodyExp → do
    body ← go bodyExp
    recs ←
      bindings & foldMapM \case
        IR.Standalone (_ann, name, expr) →
          DList.singleton . Lua.local1 (fromName name) <$> goExp expr
        IR.RecursiveGroup grp → do
          let binds =
                toList grp <&> \(_ann, fromName → name, _) →
                  Lua.Local
                    ( if Set.member (qualifyName modname name) topLevelNames
                        then qualifyName modname name
                        else name
                    )
                    Nothing
          assignments ← forM (toList grp) \(_ann, fromName → name, expr) →
            goExp expr
              <&> Lua.assign
                ( Lua.VarName
                    ( if Set.member (qualifyName modname name) topLevelNames
                        then qualifyName modname name
                        else name
                    )
                )
          pure $ DList.fromList binds <> DList.fromList assignments
    pure . Left $
      recs <> either id (DList.singleton . Lua.return) body
  IR.IfThenElse _ann cond th el → do
    thenExp ← go th
    elseExp ← go el
    condExp ← goExp cond
    let
      thenBranch = either id (pure . Lua.return) thenExp
      elseBranch = either id (pure . Lua.return) elseExp
    pure $
      Left $
        DList.singleton $
          Lua.ifThenElse condExp (toList thenBranch) (toList elseBranch)
  IR.Exception _ann msg →
    pure . Right $ Lua.error msg
  IR.ForeignImport _ann _moduleName path annotatedNames → do
    let foreignNames = fromName <<$>> annotatedNames
    Foreign.Source {header, exports} ←
      Oops.hoistEither =<< liftIO do
        left LinkerErrorForeign
          <$> Foreign.parseForeignSource (untag foreigns) path
    let foreignExports ∷ Lua.Exp =
          Lua.table
            [ Lua.tableRowNV name (Lua.ForeignSourceExp src)
            | (key, src) ← toList exports
            , -- Export tables can contain Lua-reserved words as keys
            -- for example: `{ ["for"] = 42 }`
            let name = Key.toSafeName key
            , name `elem` fmap snd foreignNames
            ]
    pure case header of
      Nothing → Right foreignExports
      Just fh →
        Left $
          DList.fromList
            [ Lua.ForeignSourceStat fh
            , Lua.return foreignExports
            ]
 where
  go ∷ IR.Exp → LuaM e (Either Lua.Chunk Lua.Exp)
  go = fromIR foreigns topLevelNames modname

  goExp ∷ IR.Exp → LuaM e Lua.Exp
  goExp = asExpression <<$>> go

keyCtor ∷ Lua.Exp
keyCtor = Lua.String "$ctor"

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

qualifyName ∷ ModuleName → Lua.Name → Lua.Name
qualifyName modname = Name.join2 (fromModuleName modname)
