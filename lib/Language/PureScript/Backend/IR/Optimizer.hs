module Language.PureScript.Backend.IR.Optimizer where

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Query (collectBoundNames)
import Language.PureScript.Backend.IR.Types
  ( Annotated
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
  , unAnn
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

renameShadowedNamesInExpr ∷ RenamesInScope → RawExp Identity → RawExp Identity
renameShadowedNamesInExpr scope = go
 where
  go = \case
    IR.LiteralInt i →
      IR.LiteralInt i
    IR.LiteralFloat f →
      IR.LiteralFloat f
    IR.LiteralString s →
      IR.LiteralString s
    IR.LiteralChar c →
      IR.LiteralChar c
    IR.LiteralBool b →
      IR.LiteralBool b
    IR.LiteralArray as →
      IR.LiteralArray (go <<$>> as)
    IR.LiteralObject ps →
      IR.LiteralObject ((go <$>) <<$>> ps)
    IR.ReflectCtor a →
      IR.ReflectCtor (go <$> a)
    IR.Eq a b →
      IR.Eq (go <$> a) (go <$> b)
    IR.DataArgumentByIndex index a →
      IR.DataArgumentByIndex index (go <$> a)
    IR.ArrayLength a →
      IR.ArrayLength (go <$> a)
    IR.ArrayIndex a index →
      IR.ArrayIndex (go <$> a) index
    IR.ObjectProp a prop →
      IR.ObjectProp (go <$> a) prop
    IR.ObjectUpdate a ps →
      IR.ObjectUpdate (go <$> a) ((go <$>) <<$>> ps)
    IR.Abs param body →
      IR.Abs param' (renameShadowedNamesInExpr scope' <$> body)
     where
      (param', scope') =
        case IR.unAnn param of
          IR.ParamUnused →
            (param, scope)
          IR.ParamNamed name →
            first
              (pure . IR.ParamNamed)
              (withScopedName (IR.unAnn body) scope name)
    IR.App a b →
      IR.App (go <$> a) (go <$> b)
    IR.Ref qname index →
      case qname of
        IR.Local lname
          | Just renames ← Map.lookup lname scope
          , Just rename ← renames !!? fromIntegral (IR.unIndex index) →
              IR.Ref (IR.Local rename) 0
        _ → IR.Ref qname index
    IR.Let binds body →
      IR.Let (NE.fromList (reverse binds')) body'
     where
      scope' ∷ RenamesInScope
      binds' ∷ [Grouping (Identity Name, Identity Exp)]
      (scope', binds') = foldl' f (scope, []) (toList binds)
      f
        ∷ (RenamesInScope, [Grouping (Identity Name, Identity Exp)])
        → Grouping (Identity Name, Identity Exp)
        → (RenamesInScope, [Grouping (Identity Name, Identity Exp)])
      f (sc, bs) = \case
        Standalone (IR.unAnn → name, expr) →
          withScopedName (IR.unAnn expr) sc name & \(name', sc') →
            let expr' = renameShadowedNamesInExpr sc <$> expr
             in (sc', Standalone (pure name', expr') : bs)
        RecursiveGroup (toList → recGroup) →
          (: bs) . RecursiveGroup . NE.fromList <$> foldl' g (sc, []) recGroup
         where
          g
            ∷ (RenamesInScope, [(Identity Name, Identity Exp)])
            → (Identity Name, Identity Exp)
            → (RenamesInScope, [(Identity Name, Identity Exp)])
          g (sc', recBinds) (IR.unAnn → name, expr) =
            withScopedName (IR.unAnn expr) sc' name & \(name', sc'') →
              let expr' = renameShadowedNamesInExpr sc' <$> expr
               in (sc'', (pure name', expr') : recBinds)
      body' = renameShadowedNamesInExpr scope' <$> body
    IR.IfThenElse i t e →
      IR.IfThenElse (go <$> i) (go <$> t) (go <$> e)
    IR.Ctor aty mn ty ctr fs →
      IR.Ctor aty mn ty ctr fs
    IR.Exception m →
      IR.Exception m
    IR.ForeignImport m p ns →
      IR.ForeignImport m p ns
   where
    withScopedName ∷ Exp → Map Name [Name] → Name → (Name, Map Name [Name])
    withScopedName e sc name = case Map.lookup name sc of
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

optimizedExpression ∷ Exp → Exp
optimizedExpression =
  rewriteExpTopDown $
    constantFolding
      `thenRewrite` removeUnreachableThenBranch
      `thenRewrite` removeUnreachableElseBranch
      `thenRewrite` removeIfWithEqualBranches
      `thenRewrite` inlineLocalBindings

constantFolding ∷ RewriteRule
constantFolding =
  pure . \case
    Eq (unAnn → LiteralBool a) (unAnn → LiteralBool b) →
      Rewritten Stop $ literalBool $ a == b
    Eq (unAnn → LiteralInt a) (unAnn → LiteralInt b) →
      Rewritten Stop $ literalBool $ a == b
    Eq (unAnn → LiteralFloat a) (unAnn → LiteralFloat b) →
      Rewritten Stop $ literalBool $ a == b
    Eq (unAnn → LiteralChar a) (unAnn → LiteralChar b) →
      Rewritten Stop $ literalBool $ a == b
    Eq (unAnn → LiteralString a) (unAnn → LiteralString b) →
      Rewritten Stop $ literalBool $ a == b
    _ → NoChange

removeIfWithEqualBranches ∷ RewriteRule
removeIfWithEqualBranches e =
  pure case e of
    IfThenElse _cond thenBranch elseBranch
      | thenBranch == elseBranch →
          Rewritten Recurse (unAnn thenBranch)
    _ → NoChange

removeUnreachableThenBranch ∷ RewriteRule
removeUnreachableThenBranch e =
  pure case e of
    IfThenElse (unAnn → LiteralBool False) _unreachable elseBranch →
      Rewritten Recurse (unAnn elseBranch)
    _ → NoChange

removeUnreachableElseBranch ∷ RewriteRule
removeUnreachableElseBranch e = pure case e of
  IfThenElse (unAnn → LiteralBool True) thenBranch _unreachable →
    Rewritten Recurse (unAnn thenBranch)
  _ → NoChange

-- Inlining is a tricky business:
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf

inlineLocalBindings ∷ RewriteRule
inlineLocalBindings =
  pure . \case
    Let groupings body →
      Rewritten Recurse . Let groupings $
        foldr inlineLocalBinding body groupings
    _ → NoChange

inlineLocalBinding
  ∷ Grouping (Identity Name, Annotated Identity RawExp)
  → Annotated Identity RawExp
  → Annotated Identity RawExp
inlineLocalBinding grouping body =
  case grouping of
    RecursiveGroup _grp → body -- TODO: inline recursive bindings?
    Standalone (unAnn → name, unAnn → inlinee) →
      if isInlinableExpr inlinee
        || sum (countFreeRef (Local name) <$> body) == 1
        then substitute (Local name) 0 inlinee <$> body
        else body

isInlinableExpr ∷ Exp → Bool
isInlinableExpr expr = isRef expr || isNonRecursiveLiteral expr
 where
  isRef ∷ Exp → Bool
  isRef = \case
    Ref {} → True
    _ → False
