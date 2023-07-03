module Language.PureScript.Backend.IR.Optimizer where

import Data.Map qualified as Map
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Linker (UberModule (..))
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

optimizedUberModule ∷ UberModule → UberModule
optimizedUberModule = idempotently $ DCE.eliminateDeadCode . optimizeModule

idempotently ∷ Eq a ⇒ (a → a) → a → a
idempotently = fix $ \i f a →
  let a' = f a
   in if a' == a then a else i f a'

--  in if a' == a
--       then trace ("\n\nFIXPOINT\n" <> {- shower a' <> -} "\n") a
--       else trace ("\n\nRETRYING\n" <> {- shower a' <> -} "\n") $ i f a'

optimizeModule ∷ UberModule → UberModule
optimizeModule UberModule {..} =
  UberModule
    { uberModuleBindings = uberModuleBindings'
    , uberModuleExports = uberModuleExports'
    , ..
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
