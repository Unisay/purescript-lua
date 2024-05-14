module Language.PureScript.Backend.IR.Query where

import Control.Lens.Plated (transformMOf)
import Control.Monad.Trans.Accum (add, execAccum)
import Data.Map qualified as Map
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MMap
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Names
  ( Name (Name)
  , Qualified (Imported, Local)
  , runModuleName
  )
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Grouping (..)
  , Index
  , Parameter (..)
  , RawExp (..)
  , bindingNames
  , listGrouping
  , subexpressions
  )
import Language.PureScript.Backend.IR.Types qualified as IR
import Language.PureScript.Names (runtimeLazyName)

countFreeRefs ∷ RawExp ann → Map (Qualified Name) Natural
countFreeRefs = fmap getSum . MMap.toMap . countFreeRefs' mempty
 where
  countFreeRefs'
    ∷ Map (Qualified Name) Index
    → RawExp ann
    → MonoidMap (Qualified Name) (Sum Natural)
  countFreeRefs' minIndexes = \case
    Ref _ann qname index →
      if Map.findWithDefault 0 qname minIndexes <= index
        then MMap.singleton qname (Sum 1)
        else mempty
    Abs _ann param body →
      case param of
        ParamNamed _paramAnn name → countFreeRefs' minIndexes' body
         where
          minIndexes' = Map.insertWith (+) (Local name) 1 minIndexes
        ParamUnused _paramAnn → countFreeRefs' minIndexes body
    Let _ann binds body → fold (countsInBody : countsInBinds)
     where
      countsInBody = countFreeRefs' minIndexes' body
       where
        minIndexes' =
          foldr (\name → Map.insertWith (+) name 1) minIndexes $
            toList binds >>= fmap Local . bindingNames
      countsInBinds =
        toList binds >>= \case
          Standalone (_nameAnn, boundName, expr) →
            [countFreeRefs' minIndexes' expr]
           where
            minIndexes' = Map.insertWith (+) (Local boundName) 1 minIndexes
          RecursiveGroup recBinds →
            toList recBinds <&> \(_nameAnn, _boundName, expr) →
              countFreeRefs' minIndexes' expr
           where
            minIndexes' =
              foldr
                (\(_nameAnn, qName, _expr) → Map.insertWith (+) (Local qName) 1)
                minIndexes
                recBinds
    App _ann argument function →
      go argument <> go function
    LiteralArray _ann as →
      foldMap go as
    LiteralObject _ann props →
      foldMap (go . snd) props
    ReflectCtor _ann a →
      go a
    DataArgumentByIndex _ann _idx a →
      go a
    Eq _ann a b →
      go a <> go b
    ArrayLength _ann a →
      go a
    ArrayIndex _ann a _indx →
      go a
    ObjectProp _ann a _prop →
      go a
    ObjectUpdate _ann a patches →
      go a <> foldMap (go . snd) patches
    IfThenElse _ann p th el →
      go p <> go th <> go el
    -- Terminals:
    LiteralInt {} → mempty
    LiteralBool {} → mempty
    LiteralFloat {} → mempty
    LiteralString {} → mempty
    LiteralChar {} → mempty
    Ctor {} → mempty
    Exception {} → mempty
    ForeignImport {} → mempty
   where
    go = countFreeRefs' minIndexes

countFreeRef ∷ Qualified Name → RawExp ann → Natural
countFreeRef name = Map.findWithDefault 0 name . countFreeRefs

usesRuntimeLazy ∷ UberModule → Bool
usesRuntimeLazy UberModule {uberModuleBindings, uberModuleExports} =
  getAny $
    foldMap
      (foldMap (\(_qname, e) → Any (findRuntimeLazyInExpr e)) . listGrouping)
      uberModuleBindings
      <> foldMap (Any . findRuntimeLazyInExpr . snd) uberModuleExports

findRuntimeLazyInExpr ∷ Exp → Bool
findRuntimeLazyInExpr expr =
  countFreeRef (Local (Name runtimeLazyName)) expr > 0

usesPrimModule ∷ UberModule → Bool
usesPrimModule UberModule {uberModuleBindings, uberModuleExports} =
  getAny $
    foldMap
      (foldMap (\(_qname, e) → Any (findPrimModuleInExpr e)) . listGrouping)
      uberModuleBindings
      <> foldMap (Any . findPrimModuleInExpr . snd) uberModuleExports

findPrimModuleInExpr ∷ Exp → Bool
findPrimModuleInExpr expr =
  Map.keys (countFreeRefs expr) & any \case
    Local _name → False
    Imported moduleName _name → runModuleName moduleName == "Prim"

collectBoundNames ∷ Exp → Set Name
collectBoundNames =
  (`execAccum` Set.empty) . transformMOf subexpressions \e →
    case e of
      IR.Abs _ann (IR.ParamNamed _paramAnn name) _body →
        e <$ add (Set.singleton name)
      IR.Let _ann groupings _body →
        e <$ add do
          Set.fromList
            [iname | grouping ← toList groupings, iname ← bindingNames grouping]
      _ → pure e
