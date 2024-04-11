module Language.PureScript.Backend.IR.Optimizer where

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Query (collectBoundNames)
import Language.PureScript.Backend.IR.Types
  ( Ann
  , Exp
  , Grouping (..)
  , Name (..)
  , QName (..)
  , Qualified (..)
  , RawExp (..)
  , RewriteMod (..)
  , RewriteRule
  , Rewritten (..)
  , bindingExprs
  , countFreeRef
  , countFreeRefs
  , isNonRecursiveLiteral
  , literalBool
  , qualifiedQName
  , rewriteExpTopDown
  , substitute
  , thenRewrite
  )
import Language.PureScript.Backend.IR.Types qualified as IR

optimizedUberModule ∷ UberModule → UberModule
optimizedUberModule =
  renameShadowedNames . idempotently (DCE.eliminateDeadCode . optimizeModule)

renameShadowedNames ∷ UberModule → UberModule
renameShadowedNames uberModule =
  uberModule
    { uberModuleExports =
        renameShadowedNamesInExpr mempty <<$>> uberModuleExports uberModule
    }

type RenamesInScope = Map Name [Name]

renameShadowedNamesInExpr ∷ RenamesInScope → Exp → Exp
renameShadowedNamesInExpr scope = go
 where
  go ∷ Exp → Exp
  go = \case
    IR.LiteralInt ann i →
      IR.LiteralInt ann i
    IR.LiteralFloat ann f →
      IR.LiteralFloat ann f
    IR.LiteralString ann s →
      IR.LiteralString ann s
    IR.LiteralChar ann c →
      IR.LiteralChar ann c
    IR.LiteralBool ann b →
      IR.LiteralBool ann b
    IR.LiteralArray ann as →
      IR.LiteralArray ann (go <$> as)
    IR.LiteralObject ann ps →
      IR.LiteralObject ann (go <<$>> ps)
    IR.ReflectCtor ann a →
      IR.ReflectCtor ann (go a)
    IR.Eq ann a b →
      IR.Eq ann (go a) (go b)
    IR.DataArgumentByIndex ann index a →
      IR.DataArgumentByIndex ann index (go a)
    IR.ArrayLength ann a →
      IR.ArrayLength ann (go a)
    IR.ArrayIndex ann a index →
      IR.ArrayIndex ann (go a) index
    IR.ObjectProp ann a prop →
      IR.ObjectProp ann (go a) prop
    IR.ObjectUpdate ann a ps →
      IR.ObjectUpdate ann (go a) (go <<$>> ps)
    IR.Abs ann param body →
      IR.Abs ann param' (renameShadowedNamesInExpr scope' body)
     where
      (param', scope') =
        case param of
          IR.ParamUnused _ann → (param, scope)
          IR.ParamNamed paramAnn name →
            first (IR.ParamNamed paramAnn) (withScopedName body scope name)
    IR.App ann a b →
      IR.App ann (go a) (go b)
    IR.Ref ann qname index →
      case qname of
        IR.Local lname
          | Just renames ← Map.lookup lname scope
          , Just rename ← renames !!? fromIntegral (IR.unIndex index) →
              IR.Ref ann (IR.Local rename) 0
        _ → IR.Ref ann qname index
    IR.Let ann binds body →
      IR.Let ann (NE.fromList (reverse binds')) body'
     where
      scope' ∷ RenamesInScope
      binds' ∷ [Grouping (Ann, Name, Exp)]
      (scope', binds') = foldl' f (scope, []) (toList binds)
      f
        ∷ (RenamesInScope, [Grouping (Ann, Name, Exp)])
        → Grouping (Ann, Name, Exp)
        → (RenamesInScope, [Grouping (Ann, Name, Exp)])
      f (sc, bs) = \case
        Standalone (ann', name, expr) →
          withScopedName expr sc name & \(name', sc') →
            let expr' = renameShadowedNamesInExpr sc expr
             in (sc', Standalone (ann', name', expr') : bs)
        RecursiveGroup (toList → recGroup) →
          (: bs) . RecursiveGroup . NE.fromList <$> foldl' g (sc, []) recGroup
         where
          g
            ∷ (RenamesInScope, [(Ann, Name, Exp)])
            → (Ann, Name, Exp)
            → (RenamesInScope, [(Ann, Name, Exp)])
          g (sc', recBinds) (ann', name, expr) =
            withScopedName expr sc' name & \(name', sc'') →
              let expr' = renameShadowedNamesInExpr sc' expr
               in (sc'', (ann', name', expr') : recBinds)
      body' = renameShadowedNamesInExpr scope' body
    IR.IfThenElse ann i t e →
      IR.IfThenElse ann (go i) (go t) (go e)
    IR.Ctor ann aty mn ty ctr fs →
      IR.Ctor ann aty mn ty ctr fs
    IR.Exception ann m →
      IR.Exception ann m
    IR.ForeignImport ann m p ns →
      IR.ForeignImport ann m p ns
   where
    withScopedName ∷ Exp → Map Name [Name] → Name → (Name, Map Name [Name])
    withScopedName e sc name =
      case Map.lookup name sc of
        Nothing → (name, Map.insert name [name] sc)
        Just renames →
          ( rename
          , Map.insert rename [] $ Map.insert name (rename : renames) sc
          )
         where
          nextIndex = length renames
          usedNames = Map.keysSet sc <> collectBoundNames e
          rename = uniqueName usedNames name nextIndex

    uniqueName ∷ Set Name → Name → Int → Name
    uniqueName usedNames n i =
      let nextName = Name (nameToText n <> show i)
       in if Set.member nextName usedNames
            then uniqueName usedNames n (i + 1)
            else nextName

idempotently ∷ Eq a ⇒ (a → a) → a → a
idempotently = fix $ \i f a →
  let a' = f a
   in if a' == a then a else i f a'

-- in if a' == a
--   then trace ("\n\nFIXPOINT\n" <> {- shower a' <> -} "\n") a
--   else trace ("\n\nRETRYING\n" <> {- shower a' <> -} "\n") $ i f a'

optimizeModule ∷ UberModule → UberModule
optimizeModule UberModule {..} =
  UberModule
    { uberModuleBindings = uberModuleBindings'
    , uberModuleExports = uberModuleExports'
    }
 where
  (uberModuleBindings', uberModuleExports') =
    fmap optimizedExpression
      <<$>> foldr withBinding ([], uberModuleExports) uberModuleBindings

  withBinding
    ∷ Grouping (QName, Exp)
    → ([Grouping (QName, Exp)], [(Name, Exp)])
    → ([Grouping (QName, Exp)], [(Name, Exp)])
  withBinding binding (bindings, exports) =
    case binding of
      Standalone (qname, optimizedExpression → expr) →
        if isInlinableExpr expr || isUsedOnce qname
          then
            ( substituteInBindings qname expr bindings
            , substituteInExports qname expr exports
            )
          else (Standalone (qname, expr) : bindings, exports)
       where
        isUsedOnce name =
          1 == Map.findWithDefault 0 (qualifiedQName name) uberModuleFreeRefs
        uberModuleFreeRefs ∷ Map (Qualified Name) Natural =
          foldr
            (\e m → Map.unionWith (+) m (countFreeRefs e))
            mempty
            uberModuleExprs
        uberModuleExprs =
          (bindingExprs =<< uberModuleBindings) <> map snd exports
      RecursiveGroup recGroup →
        ( RecursiveGroup (optimizedExpression <<$>> recGroup) : bindings
        , exports
        )

substituteInBindings
  ∷ QName
  -- ^ Substitute this qualified name
  → Exp
  -- ^ For this expression
  → [Grouping (QName, Exp)]
  -- ^ inside these bindings
  → [Grouping (QName, Exp)]
substituteInBindings qname inlinee = map \case
  Standalone (qname', expr') →
    Standalone (qname', substitute (qualifiedQName qname) 0 inlinee expr')
  RecursiveGroup recGroup →
    RecursiveGroup $ substitute (qualifiedQName qname) 0 inlinee <<$>> recGroup

substituteInExports ∷ QName → Exp → [(Name, Exp)] → [(Name, Exp)]
substituteInExports qname inlinee = map \case
  (name, expr) → (name, substitute (qualifiedQName qname) 0 inlinee expr)

optimizedExpression ∷ RawExp Ann → RawExp Ann
optimizedExpression =
  rewriteExpTopDown $
    constantFolding
      `thenRewrite` removeUnreachableThenBranch
      `thenRewrite` removeUnreachableElseBranch
      `thenRewrite` removeIfWithEqualBranches
      `thenRewrite` inlineLocalBindings

constantFolding ∷ RewriteRule Ann
constantFolding =
  pure . \case
    Eq _ (LiteralBool _ a) (LiteralBool _ b) →
      Rewritten Stop $ literalBool $ a == b
    Eq _ (LiteralInt _ a) (LiteralInt _ b) →
      Rewritten Stop $ literalBool $ a == b
    Eq _ (LiteralFloat _ a) (LiteralFloat _ b) →
      Rewritten Stop $ literalBool $ a == b
    Eq _ (LiteralChar _ a) (LiteralChar _ b) →
      Rewritten Stop $ literalBool $ a == b
    Eq _ (LiteralString _ a) (LiteralString _ b) →
      Rewritten Stop $ literalBool $ a == b
    _ → NoChange

removeIfWithEqualBranches ∷ RewriteRule Ann
removeIfWithEqualBranches e =
  pure case e of
    IfThenElse _ann _cond thenBranch elseBranch
      | thenBranch == elseBranch →
          Rewritten Recurse thenBranch
    _ → NoChange

removeUnreachableThenBranch ∷ RewriteRule Ann
removeUnreachableThenBranch e =
  pure case e of
    IfThenElse _ann (LiteralBool _ False) _unreachable elseBranch →
      Rewritten Recurse elseBranch
    _ → NoChange

removeUnreachableElseBranch ∷ RewriteRule Ann
removeUnreachableElseBranch e = pure case e of
  IfThenElse _ann (LiteralBool _ True) thenBranch _unreachable →
    Rewritten Recurse thenBranch
  _ → NoChange

-- Inlining is a tricky business:
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf

inlineLocalBindings ∷ RewriteRule Ann
inlineLocalBindings =
  pure . \case
    Let ann groupings body →
      Rewritten Recurse . Let ann groupings $
        foldr inlineLocalBinding body groupings
    _ → NoChange

inlineLocalBinding ∷ Grouping (Ann, Name, Exp) → Exp → Exp
inlineLocalBinding grouping body =
  case grouping of
    RecursiveGroup _grp → body -- Not going to inline recursive bindings
    Standalone (_ann, Local → name, inlinee) →
      if isInlinableExpr inlinee || countFreeRef name body == 1
        then substitute name 0 inlinee body
        else body

isInlinableExpr ∷ Exp → Bool
isInlinableExpr expr =
  hasInlineAnnotation expr || isRef expr || isNonRecursiveLiteral expr
 where
  isRef ∷ RawExp a → Bool
  isRef = \case
    Ref {} → True
    _ → False

  hasInlineAnnotation ∷ Exp → Bool
  hasInlineAnnotation _ = False
