module Language.PureScript.Backend.IR.Optimizer where

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.DCE (eliminateDeadCode)
import Language.PureScript.Backend.IR.Inliner (Annotation (..))
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Names
  ( Name (..)
  , QName
  , Qualified (Local)
  , qualifiedQName
  )
import Language.PureScript.Backend.IR.Query
  ( collectBoundNames
  , countFreeRef
  , countFreeRefs
  )
import Language.PureScript.Backend.IR.Types
  ( Ann
  , Exp
  , Grouping (..)
  , Parameter (..)
  , RawExp (..)
  , RewriteMod (..)
  , RewriteRule
  , Rewritten (..)
  , bindingExprs
  , getAnn
  , isNonRecursiveLiteral
  , literalBool
  , rewriteExpTopDown
  , substitute
  , thenRewrite
  , unIndex
  )

optimizedUberModule ∷ UberModule → UberModule
optimizedUberModule =
  idempotently (eliminateDeadCode . optimizeModule)
    -- by merging foreign bindings into the main bindings, we can
    -- unblock even more optimizations, e.g. inline foreign bindings.
    >>> mergeForeignsIntoBindings
    >>> idempotently (eliminateDeadCode . optimizeModule)
    >>> renameShadowedNames

mergeForeignsIntoBindings ∷ UberModule → UberModule
mergeForeignsIntoBindings uberModule@UberModule {..} =
  uberModule
    { uberModuleForeigns = []
    , uberModuleBindings =
        map Standalone uberModuleForeigns <> uberModuleBindings
    }

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
    LiteralInt ann i →
      LiteralInt ann i
    LiteralFloat ann f →
      LiteralFloat ann f
    LiteralString ann s →
      LiteralString ann s
    LiteralChar ann c →
      LiteralChar ann c
    LiteralBool ann b →
      LiteralBool ann b
    LiteralArray ann as →
      LiteralArray ann (go <$> as)
    LiteralObject ann ps →
      LiteralObject ann (go <<$>> ps)
    ReflectCtor ann a →
      ReflectCtor ann (go a)
    Eq ann a b →
      Eq ann (go a) (go b)
    DataArgumentByIndex ann index a →
      DataArgumentByIndex ann index (go a)
    ArrayLength ann a →
      ArrayLength ann (go a)
    ArrayIndex ann a index →
      ArrayIndex ann (go a) index
    ObjectProp ann a prop →
      ObjectProp ann (go a) prop
    ObjectUpdate ann a ps →
      ObjectUpdate ann (go a) (go <<$>> ps)
    Abs ann param body →
      Abs ann param' (renameShadowedNamesInExpr scope' body)
     where
      (param', scope') =
        case param of
          ParamUnused _ann → (param, scope)
          ParamNamed paramAnn name →
            first (ParamNamed paramAnn) (withScopedName body scope name)
    App ann a b →
      App ann (go a) (go b)
    Ref ann qname index →
      case qname of
        Local lname
          | Just renames ← Map.lookup lname scope
          , Just rename ← renames !!? fromIntegral (unIndex index) →
              Ref ann (Local rename) 0
        _ → Ref ann qname index
    Let ann binds body →
      Let ann (NE.fromList (reverse binds')) body'
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
    IfThenElse ann i t e →
      IfThenElse ann (go i) (go t) (go e)
    Ctor ann aty mn ty ctr fs →
      Ctor ann aty mn ty ctr fs
    Exception ann m →
      Exception ann m
    ForeignImport ann m p ns →
      ForeignImport ann m p ns
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

--       if a' == a
--         then tr "FIXPOINT" a a
--         else tr "RETRYING" a' (i f a')
--  where
--   tr ∷ Show x ⇒ String → x → y → y
--   tr l x = trace ("\n\n" <> l <> "\n" <> (toString . pShow) x <> "\n")

optimizeModule ∷ UberModule → UberModule
optimizeModule UberModule {..} =
  UberModule
    { uberModuleForeigns
    , uberModuleBindings = uberModuleBindings'
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

optimizedExpression ∷ Exp → Exp
optimizedExpression =
  rewriteExpTopDown $
    constantFolding
      `thenRewrite` betaReduce
      `thenRewrite` etaReduce
      `thenRewrite` betaReduceUnusedParams
      `thenRewrite` removeUnreachableThenBranch
      `thenRewrite` removeUnreachableElseBranch
      `thenRewrite` removeIfWithEqualBranches
      `thenRewrite` inlineLocalBindings

constantFolding ∷ RewriteRule Ann
constantFolding =
  pure . \case
    Eq _ (LiteralBool _ a) (LiteralBool _ b) →
      Rewritten Stop $ literalBool $ a == b
    Eq _ (LiteralBool _ True) b →
      -- 'b' must be of type Bool
      Rewritten Stop b
    Eq _ (LiteralInt _ a) (LiteralInt _ b) →
      Rewritten Stop $ literalBool $ a == b
    Eq _ (LiteralFloat _ a) (LiteralFloat _ b) →
      Rewritten Stop $ literalBool $ a == b
    Eq _ (LiteralChar _ a) (LiteralChar _ b) →
      Rewritten Stop $ literalBool $ a == b
    Eq _ (LiteralString _ a) (LiteralString _ b) →
      Rewritten Stop $ literalBool $ a == b
    _ → NoChange

-- (λx. M) N ===> M[x := N]
betaReduce ∷ RewriteRule Ann
betaReduce =
  pure . \case
    App _ (Abs _ (ParamNamed _ param) body) r →
      Rewritten Recurse $ substitute (Local param) 0 r body
    _ → NoChange

-- (λx. M x) where x not free in M ===> M
etaReduce ∷ RewriteRule Ann
etaReduce =
  pure . \case
    Abs _ (ParamNamed _ param) (App _ m (Ref _ (Local param') 0))
      | param == param' && countFreeRef (Local param) m == 0 →
          Rewritten Recurse m
    _ → NoChange

betaReduceUnusedParams ∷ RewriteRule Ann
betaReduceUnusedParams =
  pure . \case
    App _ (Abs _ (ParamUnused _) body) _arg →
      Rewritten Recurse body
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
  hasInlineAnnotation =
    getAnn >>> \case
      Just Always → True
      Just Never → False
      Nothing → False
