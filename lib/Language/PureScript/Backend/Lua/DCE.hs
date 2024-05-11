module Language.PureScript.Backend.Lua.DCE where

import Control.Lens ((%~))
import Control.Lens.Plated qualified as Plated
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.Map.Lazy qualified as Map
import Data.Map.Merge.Lazy qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.Lua.Name (Name)
import Language.PureScript.Backend.Lua.Name qualified as Name
import Language.PureScript.Backend.Lua.Traversal
  ( Annotator (..)
  , Rewrites (..)
  , annotateStatementInsideOutM
  , makeRewrites
  , rewriteStatementM
  )
import Language.PureScript.Backend.Lua.Types
  ( Ann
  , HasAnn (..)
  , annL
  , annOf
  )
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prelude hiding (exp)

data DceMode = PreserveTopLevel | PreserveReturned

type Key = Int
type NodeEdges = (Text, Key, [Key])

eliminateDeadCode ∷ DceMode → [Lua.Statement] → [Lua.Statement]
eliminateDeadCode dceMode stats = dceChunk annotatedStatements
 where
  annotatedStatements = dceAnnotatedStatements stats

  ( graph ∷ Graph
    , _nodeFromVertex ∷ Vertex → NodeEdges
    , keyToVertex ∷ Key → Maybe Vertex
    ) = graphFromEdges nodesEdges

  nodesEdges ∷ [NodeEdges]
  nodesEdges = DList.toList (adjacencyList annotatedStatements)

  dceChunk = foldMap $ toList . dceStatement
  dceStatement statement =
    case statement of
      Lua.Local dceAnn name value →
        ifKeyIsReachable $
          Lua.Local (unDceAnn dceAnn) name (dceExpression <$> value)
      Lua.Assign dceAnn variable value →
        ifKeyIsReachable $
          Lua.Assign (unDceAnn dceAnn) (dceVar variable) (dceExpression value)
      Lua.IfThenElse dceAnn i t e →
        ifKeyIsReachable $
          Lua.IfThenElse
            (unDceAnn dceAnn)
            (dceExpression i)
            (dceChunk t)
            (dceChunk e)
      Lua.Return dceAnn exp →
        Just $ Lua.Return (unDceAnn dceAnn) (dceExpression exp)
      Lua.ForeignSourceStat dceAnn s →
        Just $ Lua.ForeignSourceStat (unDceAnn dceAnn) s
   where
    key = keyOf statement
    ifKeyIsReachable preserved = do
      vertex ← keyToVertex key
      guard (Set.member vertex reachableVertices) $> preserved

  dceExpression ∷ Lua.ExpF DceAnn → Lua.Exp
  dceExpression expr = case expr of
    Lua.Nil dceAnn →
      Lua.Nil (unDceAnn dceAnn)
    Lua.Boolean dceAnn b →
      Lua.Boolean (unDceAnn dceAnn) b
    Lua.Integer dceAnn int →
      Lua.Integer (unDceAnn dceAnn) int
    Lua.Float dceAnn double →
      Lua.Float (unDceAnn dceAnn) double
    Lua.String dceAnn text →
      Lua.String (unDceAnn dceAnn) text
    Lua.Function dceAnn params body →
      Lua.Function (unDceAnn dceAnn) (dceParams params) (dceChunk body)
    Lua.TableCtor dceAnn rows →
      Lua.TableCtor (unDceAnn dceAnn) (dceTableRow <$> rows)
    Lua.UnOp dceAnn op e →
      Lua.UnOp (unDceAnn dceAnn) op (dceExpression e)
    Lua.BinOp dceAnn op e1 e2 →
      Lua.BinOp (unDceAnn dceAnn) op (dceExpression e1) (dceExpression e2)
    Lua.Var dceAnn v →
      Lua.Var (unDceAnn dceAnn) (dceVar v)
    Lua.FunctionCall dceAnn e es →
      Lua.FunctionCall
        (unDceAnn dceAnn)
        (dceExpression e)
        (dceExpression <$> es)
    Lua.ForeignSourceExp dceAnn src →
      Lua.ForeignSourceExp (unDceAnn dceAnn) src

  dceParams ∷ [Lua.ParamF DceAnn] → [Lua.Param]
  dceParams paramNodes =
    paramNodes >>= \case
      Lua.ParamUnused dceAnn → [Lua.ParamUnused (unDceAnn dceAnn)]
      p@(Lua.ParamNamed dceAnn name) → do
        vertex ← maybeToList $ keyToVertex $ keyOf p
        if Set.member vertex reachableVertices
          then [Lua.ParamNamed (unDceAnn dceAnn) name]
          else [Lua.ParamUnused (unDceAnn dceAnn)]

  dceTableRow ∷ Lua.TableRowF DceAnn → Lua.TableRow
  dceTableRow = \case
    Lua.TableRowKV dceAnn k v →
      Lua.TableRowKV (unDceAnn dceAnn) (dceExpression k) (dceExpression v)
    Lua.TableRowNV dceAnn n e →
      Lua.TableRowNV (unDceAnn dceAnn) n (dceExpression e)

  dceVar ∷ Lua.VarF DceAnn → Lua.Var
  dceVar = \case
    Lua.VarName dceAnn name →
      Lua.VarName (unDceAnn dceAnn) name
    Lua.VarIndex dceAnn e1 e2 →
      Lua.VarIndex (unDceAnn dceAnn) (dceExpression e1) (dceExpression e2)
    Lua.VarField dceAnn e name →
      Lua.VarField (unDceAnn dceAnn) (dceExpression e) name

  reachableVertices ∷ Set Vertex
  reachableVertices =
    let reachables = reachable graph
     in Set.fromList (dceEntryVertices >>= reachables)

  dceEntryVertices ∷ [Vertex]
  dceEntryVertices =
    case dceMode of
      PreserveTopLevel → mapMaybe (keyToVertex . keyOf) annotatedStatements
      PreserveReturned → case viaNonEmpty last annotatedStatements of
        Just (Lua.Return (DceAnn _ann k _scopes) exp) →
          mapMaybe keyToVertex [k, keyOf exp]
        _ → []

--------------------------------------------------------------------------------
-- Building graph from adjacency list ------------------------------------------

adjacencyList ∷ [Lua.StatementF DceAnn] → DList NodeEdges
adjacencyList = (`go` mempty)
 where
  go
    ∷ [Lua.StatementF DceAnn]
    → DList NodeEdges
    → DList NodeEdges
  go [] acc = acc
  go (statement : nextStatements) acc =
    go nextStatements $
      acc <> case statement of
        Lua.Local _ann name value →
          DList.cons
            ( "Local(" <> Name.toText name <> ")"
            , keyOf statement
            , toList
                let keys = findAssignments name nextStatements
                 in maybe keys (\expr → DList.cons (keyOf expr) keys) value
            )
            (maybe mempty expressionAdjacencyList value)
        Lua.Assign _ann variable value →
          DList.cons
            ("Assign", keyOf statement, [keyOf variable, keyOf value])
            (varAdjacencyList variable <> expressionAdjacencyList value)
        Lua.IfThenElse _ann cond th el →
          DList.cons
            ( "IfThenElse"
            , keyOf statement
            , keyOf cond : DList.toList (findReturns th <> findReturns el)
            )
            (expressionAdjacencyList cond)
            <> go th DList.empty
            <> go el DList.empty
        Lua.Return _ann e →
          DList.cons
            ("Return", keyOf statement, [keyOf e])
            (expressionAdjacencyList e)
        Lua.ForeignSourceStat {} →
          pure ("ForeignSourceStat", keyOf statement, [])

expressionAdjacencyList ∷ Lua.ExpF DceAnn → DList NodeEdges
expressionAdjacencyList expr =
  case expr of
    Lua.Nil _ann → pure ("Nil", keyOf expr, [])
    Lua.Boolean _ann _bool → pure ("Boolean", keyOf expr, [])
    Lua.Integer _ann _integer → pure ("Integer", keyOf expr, [])
    Lua.Float _ann _double → pure ("Float", keyOf expr, [])
    Lua.String _ann _text → pure ("String", keyOf expr, [])
    Lua.Function _ann params body →
      DList.cons
        ("Function", keyOf expr, DList.toList (findReturns body))
        (foldMap (paramsAdjacencyList (keyOf expr)) params <> adjacencyList body)
    Lua.TableCtor _ann rows →
      DList.cons
        ("TableCtor", keyOf expr, keyOf <$> rows)
        (foldMap rowAdjacencyList rows)
    Lua.UnOp _ann _op e →
      DList.cons ("UnOp", keyOf expr, [keyOf e]) (expressionAdjacencyList e)
    Lua.BinOp _ann _op e1 e2 →
      DList.cons
        ("BinOp", keyOf expr, [keyOf e1, keyOf e2])
        (expressionAdjacencyList e1 <> expressionAdjacencyList e2)
    Lua.Var _ann variable →
      DList.cons
        ("Var", keyOf expr, [keyOf variable])
        (varAdjacencyList variable)
    Lua.FunctionCall _ann e params →
      DList.cons
        ("FunctionCall", keyOf expr, keyOf e : map keyOf params)
        (expressionAdjacencyList e <> foldMap expressionAdjacencyList params)
    Lua.ForeignSourceExp _ann _src →
      pure ("ForeignSourceExp", keyOf expr, [])

paramsAdjacencyList ∷ Key → Lua.ParamF DceAnn → DList NodeEdges
paramsAdjacencyList fnKey param =
  case param of
    Lua.ParamUnused _ann →
      DList.empty
    Lua.ParamNamed _ann name →
      DList.singleton
        ( "ParamNamed(" <> Name.toText name <> ")"
        , keyOf param
        , [fnKey]
        )

varAdjacencyList ∷ Lua.VarF DceAnn → DList NodeEdges
varAdjacencyList variable =
  case variable of
    Lua.VarName _ann name →
      DList.singleton
        ( "VarName(Local " <> Name.toText name <> ")"
        , keyOf variable
        , toList (Map.lookup name (flatten (scopesOf variable)))
        )
    Lua.VarIndex _ann e1 e2 →
      DList.cons
        ("VarIndex", keyOf variable, [keyOf e1, keyOf e2])
        (expressionAdjacencyList e1 <> expressionAdjacencyList e2)
    Lua.VarField _ann e name →
      DList.cons
        ( "VarField(" <> Name.toText name <> ")"
        , keyOf variable
        , [keyOf e]
        )
        (expressionAdjacencyList e)

rowAdjacencyList ∷ Lua.TableRowF DceAnn → DList NodeEdges
rowAdjacencyList row =
  case row of
    Lua.TableRowKV _ann e1 e2 →
      DList.cons
        ("Lua.TableRowKV", keyOf row, [keyOf e1, keyOf e2])
        (expressionAdjacencyList e1 <> expressionAdjacencyList e2)
    Lua.TableRowNV _ann _name e →
      DList.cons
        ("Lua.TableRowNV", keyOf row, [keyOf e])
        (expressionAdjacencyList e)

--------------------------------------------------------------------------------
-- Queries ---------------------------------------------------------------------

findReturns ∷ [Lua.StatementF DceAnn] → DList Key
findReturns = fmap keyOf . findReturnStatements

findReturnStatements ∷ [Lua.StatementF DceAnn] → DList (Lua.StatementF DceAnn)
findReturnStatements = foldMap \statement →
  case statement of
    Lua.Return _ann _expr → DList.singleton statement
    Lua.IfThenElse _ann _cond th el →
      DList.cons statement (findReturnStatements th <> findReturnStatements el)
    _ → DList.empty

findAssignments ∷ Name → [Lua.StatementF DceAnn] → DList Key
findAssignments name =
  foldMap $
    Lua.S >>> Plated.para \term rs →
      case term of
        Lua.S (Lua.Assign ann (Lua.VarName _ name') _val)
          | name' == name →
              DList.cons (annKey ann) (fold rs)
        _ → fold rs

findVars ∷ Name → [Lua.StatementF DceAnn] → DList Key
findVars name =
  foldMap $
    Lua.S >>> Plated.para \term rs →
      case term of
        Lua.E (Lua.Var ann (Lua.VarName _ name'))
          | name' == name →
              DList.cons (annKey ann) (fold rs)
        _ → fold rs

--------------------------------------------------------------------------------
-- Annotating statements with graph keys ---------------------------------------

type Scope = Map Name Key

flatten ∷ [Scope] → Scope
flatten =
  foldl'
    ( Map.merge Map.preserveMissing Map.preserveMissing $
        Map.zipWithMatched \_name keyInner _keyOuter → keyInner
    )
    Map.empty

data DceAnn = DceAnn Ann Key [Scope]
  deriving stock (Eq, Show)

unDceAnn ∷ DceAnn → Ann
unDceAnn (DceAnn a _key _scope) = a

keyOf ∷ HasAnn f ⇒ f DceAnn → Key
keyOf = annKey . annOf

annKey ∷ DceAnn → Key
annKey (DceAnn _ key _) = key

scopesOf ∷ HasAnn f ⇒ f DceAnn → [Scope]
scopesOf f = let DceAnn _ann _key scopes = annOf f in scopes

dceAnnotatedStatements ∷ [Lua.Statement] → [Lua.StatementF DceAnn]
dceAnnotatedStatements statements =
  evalState (forM statements assignKeys) 0 & \keyedStatements →
    evalState @[Scope] (assignScopes keyedStatements) []

assignKeys ∷ Lua.Statement → State Key (Lua.StatementF DceAnn)
assignKeys =
  annotateStatementInsideOutM
    Annotator
      { withAnn = \a → state \key → (DceAnn a key mempty, key + 1)
      , annotateStat = pure
      , annotateExp = pure
      , annotateVar = pure
      , annotateParam = pure
      , annotateRow = pure
      }

assignScopes
  ∷ ∀ m. MonadScopes m ⇒ [Lua.StatementF DceAnn] → m [Lua.StatementF DceAnn]
assignScopes = traverse do
  rewriteStatementM
    makeRewrites
      { beforeStat = beforeStat
      , beforeExpr = beforeExpr
      , beforeVar = updateScopes
      , beforeRow = updateScopes
      , afterStat = afterStat
      }
 where
  beforeStat ∷ Lua.StatementF DceAnn → m (Lua.StatementF DceAnn)
  beforeStat stat =
    case stat of
      Lua.Local (DceAnn a key _scopes) name value → do
        scopes ← addName name key
        pure $ Lua.Local (DceAnn a key (toList scopes)) name value
      Lua.IfThenElse (DceAnn a key _scopes) p t e → do
        t' ← addScope $> t
        e' ← addScope $> e
        scopes ← getScopes
        pure $ Lua.IfThenElse (DceAnn a key (toList scopes)) p t' e'
      _ → pure stat

  afterStat ∷ Lua.StatementF DceAnn → m (Lua.StatementF DceAnn)
  afterStat statement =
    case statement of
      Lua.Return {} → dropScope $> statement
      _ → pure statement

  beforeExpr ∷ Lua.ExpF DceAnn → m (Lua.ExpF DceAnn)
  beforeExpr expr =
    case expr of
      Lua.Function (DceAnn ann key _scopes) argNodes body → do
        _ ← addScope
        for_ argNodes \param →
          case param of
            Lua.ParamUnused _ann → pass
            Lua.ParamNamed _ann name → void $ addName name (keyOf param)
        getScopes <&> \scopes →
          Lua.Function (DceAnn ann key (toList scopes)) argNodes body
      _ → pure expr

  updateScopes ∷ HasAnn f ⇒ f DceAnn → m (f DceAnn)
  updateScopes f = do
    scopes ← getScopes
    pure $ f & annL %~ \(DceAnn a k _scopes) → DceAnn a k scopes

class Monad m ⇒ MonadScopes m where
  addName ∷ Name → Key → m (NonEmpty Scope)
  addScope ∷ m (NonEmpty Scope)
  dropScope ∷ m ()
  getScopes ∷ m [Scope]

instance Monad m ⇒ MonadScopes (StateT [Scope] m) where
  addName name key = do
    scopes ← get
    let scopes' = case scopes of
          [] → Map.singleton name key :| []
          inner : outer → Map.insert name key inner :| outer
    put $ toList scopes'
    pure scopes'

  addScope = do
    scopes ← get
    let scopes' = Map.empty :| scopes
    put $ toList scopes'
    pure scopes'

  dropScope = modify' \case
    [] → []
    _ : remainingScopes → remainingScopes

  getScopes = get
