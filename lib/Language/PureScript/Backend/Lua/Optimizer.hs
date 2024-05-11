module Language.PureScript.Backend.Lua.Optimizer where

import Control.Lens ((^?))
import Control.Lens.Plated (children)
import Control.Lens.Plated qualified as Plated
import Control.Monad.Trans.Accum (Accum, add, execAccum)
import Data.DList (DList)
import Data.DList qualified as DL
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Tree (Tree (..), foldTree)
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Traversal
  ( everywhereInChunkM
  , everywhereStat
  , everywhereStatM
  )
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prelude hiding (return)

optimizeChunk ∷ Lua.Chunk → Lua.Chunk
optimizeChunk = idempotently optimizeChunkOnce

idempotently ∷ Eq a ⇒ (a → a) → a → a
idempotently = fix $ \i f a →
  let a' = f a
   in if a' == a then a else i f a'

{- | Given a function @f@ and an initial value @t@, applies @f@ to @t@ once,
and in case of Nothing it terminates, otherwise repeatedly applies @f@
to the result of the previous application until it no longer changes.
-}
everywhere ∷ (t → Maybe t) → t → Maybe t
everywhere f t =
  case f t of
    Nothing → Nothing
    Just t' → everywhereUntilNothing f t'
 where
  everywhereUntilNothing ∷ (t → Maybe t) → t → Maybe t
  everywhereUntilNothing g s = maybe (Just s) (everywhereUntilNothing g) (g s)

optimizeChunkOnce ∷ Lua.Chunk → Lua.Chunk
optimizeChunkOnce = go DL.empty . toList
 where
  go ∷ Lua.Chunk → [Lua.Statement] → Lua.Chunk
  go optimizedStats remainingStats =
    case remainingStats of
      [] → optimizedStats
      stat : stats →
        let (optimizedStat, remainingStats') = optimizeStatement stat stats
         in go (DL.snoc optimizedStats optimizedStat) remainingStats'

optimizeStatement
  ∷ Lua.Statement → [Lua.Statement] → (Lua.Statement, [Lua.Statement])
optimizeStatement currentStat nextStats =
  case currentStat of
    Lua.Assign
      ann
      var
      (Lua.Function _ args [Lua.Return _ (Lua.Function _ innerArgs innerBody)])
        | Just nextStats' ← everywhere (rewriteCurried var) nextStats →
            ( go $
                Lua.Assign
                  ann
                  var
                  (Lua.functionDef (args ++ innerArgs) innerBody)
            , go <$> nextStats'
            )
    _stat → (go currentStat, go <$> nextStats)
 where
  go = everywhereStat identity optimizeExpression

data AppliedHow = Unknown | NotApplied | AppliedOnce | AppliedAtLeastTwice
  deriving stock (Eq, Show, Enum, Bounded)

instance Semigroup AppliedHow where
  Unknown <> b = b
  a <> Unknown = a
  a <> b = if fromEnum a < fromEnum b then a else b

instance Monoid AppliedHow where
  mempty = Unknown

pattern NestedCall
  ∷ Lua.VarF ann
  -- ^ The var inside the inner function call
  → ann
  -- ^ The annotation of the outer function call
  → [Lua.ExpF ann]
  -- ^ The arguments of the outer function call
  → [Lua.ExpF ann]
  -- ^ The arguments of the inner function call
  → Lua.ExpF ann
  -- ^ The body of the inner function call
  → Lua.TermF ann
  -- ^ The outer function call
pattern NestedCall innerVar outerAnn outerArgs innerArgs innerCall ←
  Lua.E
    ( Lua.FunctionCall
        outerAnn
        innerCall@( Lua.FunctionCall
                      _innerAnn
                      (Lua.Var _varAnn innerVar)
                      innerArgs
                    )
        outerArgs
      )

rewriteCurried ∷ Lua.Var → [Lua.Statement] → Maybe [Lua.Statement]
rewriteCurried var (map Lua.S → statTerms) =
  case appliedHow var statTerms of
    Unknown → Nothing
    NotApplied → Nothing
    AppliedOnce → Nothing
    AppliedAtLeastTwice →
      Just $ mapMaybe ((^? Lua._S) . rewriteCurriedTerm var 2) statTerms

appliedHow ∷ Lua.Var → [Lua.Term] → AppliedHow
appliedHow var terms = foldMap appliedHowInTerm terms
 where
  appliedHowInTerm =
    foldTree (\x xs → fold (x : xs)) . Plated.para \term subterms →
      case term of
        NestedCall var' _outerAnn _outerArgs _innerArgs _innerCall
          | var == var' → Node AppliedAtLeastTwice (drop 1 subterms)
        Lua.E (Lua.FunctionCall _ (Lua.Var _ var') _args)
          | var == var' → Node AppliedOnce (drop 1 subterms)
        Lua.V var'
          | var == var' → Node NotApplied []
        _ → Node Unknown subterms

rewriteCurriedTerm ∷ Lua.Var → Int → Lua.Term → Lua.Term
rewriteCurriedTerm var numApplications term0 =
  case go term0 of Pass {resTerm} → resTerm
 where
  go ∷ Lua.Term → Res
  go term = rewriteTerm (go <$> children term)
   where
    passTerm = flip Pass Nothing

    rewriteTerm ∷ [Res] → Res
    rewriteTerm results =
      case term of
        Lua.E (Lua.Function ann params _body) →
          let body' = [s | Pass (Lua.S s) _ ← results]
           in passTerm (Lua.E (Lua.Function ann params body'))
        Lua.E (Lua.FunctionCall ann appliedExpr _args) →
          case results of
            Pass (Lua.E (Lua.Var _ var')) Nothing : passes
              | var == var' →
                  Pass
                    ( Lua.E
                        ( Lua.FunctionCall
                            ann
                            appliedExpr
                            [a | Pass (Lua.E a) _ ← passes]
                        )
                    )
                    (Just (1, numApplications))
            Pass (Lua.E subTerm) (Just (n, maxApplications)) : passes
              | succ n == maxApplications →
                  Pass
                    ( collapseFunCalls
                        (succ n)
                        ( Lua.FunctionCall
                            ann
                            subTerm
                            [a | Pass (Lua.E a) _ ← passes]
                        )
                    )
                    Nothing
            Pass _subTerm (Just (n, maxApplications)) : passes
              | n < maxApplications →
                  Pass
                    ( Lua.E
                        ( Lua.FunctionCall
                            ann
                            appliedExpr
                            [a | Pass (Lua.E a) _ ← passes]
                        )
                    )
                    (Just (succ n, maxApplications))
            Pass (Lua.E fun) _ : passes →
              passTerm . Lua.E $
                Lua.functionCall fun [a | Pass (Lua.E a) _ ← passes]
            _ →
              passTerm term
        Lua.S (Lua.Assign ann name _expr) →
          case results of
            [Pass Lua.V {} _, Pass (Lua.E expr') _] →
              passTerm (Lua.S (Lua.Assign ann name expr'))
            _ → error "Impossible subexpressions: Assign"
        Lua.S (Lua.Local ann name (Just _expr)) →
          case results of
            [Pass (Lua.E expr') _info] →
              passTerm (Lua.S (Lua.Local ann name (Just expr')))
            _ → error "Impossible subexpression: Local"
        Lua.S (Lua.IfThenElse ann _expr th el) →
          case results of
            [Pass (Lua.E expr') _info] →
              passTerm (Lua.S (Lua.IfThenElse ann expr' th el))
            _ → error "Impossible subexpressions: IfThenElse"
        Lua.S (Lua.Return ann _expr) →
          case results of
            [Pass (Lua.E expr') _info] →
              passTerm (Lua.S (Lua.Return ann expr'))
            _ → error "Impossible subexpressions: Return"
        Lua.E (Lua.UnOp ann op _expr) →
          case results of
            [Pass (Lua.E expr') _info] →
              passTerm (Lua.E (Lua.UnOp ann op expr'))
            _ → error "Impossible subexpressions: UnOp"
        Lua.E (Lua.BinOp ann op _lhs _rhs) →
          case results of
            [Pass (Lua.E lhs') _, Pass (Lua.E rhs') _] →
              passTerm (Lua.E (Lua.BinOp ann op lhs' rhs'))
            _ → error "Impossible subexpressions: BinOp"
        Lua.E (Lua.TableCtor ann _rows) →
          passTerm
            (Lua.E (Lua.TableCtor ann [r | Pass (Lua.R r) _ ← results]))
        Lua.V (Lua.VarIndex ann _lhs _rhs) →
          case results of
            [Pass (Lua.E lhs') _, Pass (Lua.E rhs') _] →
              passTerm (Lua.V (Lua.VarIndex ann lhs' rhs'))
            _ → error "Impossible subexpressions: VarIndex"
        Lua.V (Lua.VarField ann _expr field) →
          case results of
            [Pass (Lua.E expr') _] →
              passTerm (Lua.V (Lua.VarField ann expr' field))
            _ → error "Impossible subexpressions: VarField"
        Lua.R (Lua.TableRowKV ann _k _v) →
          case results of
            [Pass (Lua.E k') _, Pass (Lua.E v') _] →
              passTerm (Lua.R (Lua.TableRowKV ann k' v'))
            _ → error "Impossible subexpressions: TableRowKV"
        Lua.R (Lua.TableRowNV ann name _v) →
          case results of
            [Pass (Lua.E expr') _] →
              passTerm (Lua.R (Lua.TableRowNV ann name expr'))
            _ → error "Impossible subexpressions: TableRowNV"
        _ → passTerm term

data St = St
  { appliedExpr ∷ Maybe Lua.Exp
  , args ∷ DList Lua.Exp
  , remainingApps ∷ Int
  }
  deriving stock (Show)

collapseFunCalls ∷ Int → Lua.Exp → Lua.Term
collapseFunCalls n e
  | n < 2 = Lua.E e
  | otherwise =
      go St {appliedExpr = Nothing, args = DL.empty, remainingApps = n} e
        & \case
          St {appliedExpr = Nothing} → error "collapseFunCalls: impossible"
          St {appliedExpr = Just functionCall} → Lua.E functionCall
 where
  go ∷ St → Lua.Exp → St
  go st = \case
    Lua.FunctionCall ann expr args →
      go st expr & \st' →
        case st' of
          St {appliedExpr, remainingApps, args = args'}
            | remainingApps == 1 →
                st'
                  { remainingApps = 0
                  , appliedExpr =
                      Lua.FunctionCall ann
                        <$> appliedExpr
                        <*> pure (toList args' ++ normalizeArgs args)
                  , args = DL.empty
                  }
          St {remainingApps, args = args'}
            | remainingApps > 0 →
                st'
                  { remainingApps = pred remainingApps
                  , args = args' <> normalizeArgs (DL.fromList args)
                  }
          St {appliedExpr} →
            st'
              { appliedExpr =
                  Lua.FunctionCall ann
                    <$> appliedExpr
                    <*> pure args
              }
    expr → st {appliedExpr = Just expr}

  normalizeArgs ∷ (Foldable f, Applicative f) ⇒ f Lua.Exp → f Lua.Exp
  normalizeArgs xs = if length xs == 0 then pure Lua.nil else xs

data Res = Pass
  { resTerm ∷ Lua.Term
  , resInfo ∷ Maybe (Int, Int)
  }
  deriving stock (Show)

{-
t1 ∷ ∀ {a}. Show a ⇒ [Char] → a → a
t1 x a = trace ("\n------------<" ++ x ++ ">----------\n" ++ toString (pShow a)) a
-}

optimizeExpression ∷ Lua.Exp → Lua.Exp
optimizeExpression = foldr (>>>) identity rewriteRulesInOrder

rewriteRulesInOrder ∷ [RewriteRule]
rewriteRulesInOrder =
  [ pushDeclarationsDownTheInnerScope
  , removeScopeWhenInsideEmptyFunction
  , reduceTableDefinitionAccessor
  ]

type RewriteRule = Lua.Exp → Lua.Exp

--------------------------------------------------------------------------------
-- Rewrite rules for expressions -----------------------------------------------

pushDeclarationsDownTheInnerScope ∷ RewriteRule
pushDeclarationsDownTheInnerScope = \case
  Lua.Function _ outerArgs outerBody
    | Just lastStatement ← viaNonEmpty last outerBody
    , Lua.Return _ (Lua.Function _ innerArgs innerBody) ← lastStatement
    , declarations ← List.init outerBody
    , not (null declarations)
    , all isDeclaration declarations →
        Lua.functionDef
          outerArgs
          [Lua.return $ Lua.functionDef innerArgs (declarations <> innerBody)]
  e → e
 where
  isDeclaration ∷ Lua.Statement → Bool
  isDeclaration = \case
    Lua.Local {} → True
    Lua.Assign {} → True
    _ → False

removeScopeWhenInsideEmptyFunction ∷ RewriteRule
removeScopeWhenInsideEmptyFunction = \case
  Lua.Function
    _
    outerArgs
    [Lua.Return _ (Lua.FunctionCall _ (Lua.Function _ [] body) [])] →
      Lua.functionDef outerArgs body
  e → e

-- | Rewrites '{ foo = 1, bar = 2 }.foo' to '1'
reduceTableDefinitionAccessor ∷ RewriteRule
reduceTableDefinitionAccessor = \case
  Lua.Var _ (Lua.VarField _ (Lua.TableCtor _ rows) accessedField) →
    fromMaybe Lua.nil $
      listToMaybe
        [ fieldValue
        | Lua.TableRowNV _ tableField fieldValue ← rows
        , tableField == accessedField
        ]
  e → e

substituteVarForValue ∷ Lua.Name → Lua.Exp → Lua.Chunk → Lua.Chunk
substituteVarForValue name inlinee =
  runIdentity . everywhereInChunkM (pure . subst) pure
 where
  subst = \case
    Lua.Var _ (Lua.VarName _ varName) | varName == name → inlinee
    expr → expr

countRefs ∷ Lua.Statement → Map Lua.Name (Sum Natural)
countRefs = everywhereStatM pure countRefsInExpression >>> (`execAccum` mempty)
 where
  countRefsInExpression ∷ Lua.Exp → Accum (Map Lua.Name (Sum Natural)) Lua.Exp
  countRefsInExpression = \case
    expr@(Lua.Var _ (Lua.VarName _ name)) →
      add (Map.singleton name (Sum 1)) $> expr
    expr → pure expr
