module Language.PureScript.Backend.Lua.DeadCodeEliminator where

import Control.Monad.Trans.Accum (add, execAccum)
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
  , Visitor (..)
  , annotateStatementInsideOutM
  , makeVisitor
  , unAnnotateStatement
  , visitStatementM
  )
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prelude hiding (exp)

data DceMode = PreserveTopLevel | PreserveReturned

eliminateDeadCode :: DceMode -> Lua.Chunk -> Lua.Chunk
eliminateDeadCode dceMode chunk =
  unNodesStatement <$> dceChunk statementWithNodes
 where
  statementWithNodes :: [Lua.Annotated Node Lua.StatementF]
  statementWithNodes = makeNodesStatement chunk

  ( graph :: Graph
    , _nodeFromVertex :: Vertex -> (Label, Key, [Key])
    , keyToVertex :: Key -> Maybe Vertex
    ) = graphFromEdges (adjacencyList statementWithNodes)

  dceChunk
    :: [Lua.Annotated Node Lua.StatementF]
    -> [Lua.Annotated Node Lua.StatementF]
  dceChunk = foldMap $ toList . dceStatement

  dceStatement
    :: Lua.Annotated Node Lua.StatementF
    -> Maybe (Lua.Annotated Node Lua.StatementF)
  dceStatement vstat@(Node key scopes, statement) =
    case statement of
      Lua.Local name value ->
        ifKeyIsReachable $
          node (Lua.Local name (dceExpression <$> value))
      Lua.Assign variable value ->
        ifKeyIsReachable $
          node (Lua.Assign (dceVar variable) (dceExpression value))
      Lua.IfThenElse i t e ->
        ifKeyIsReachable $
          node (Lua.IfThenElse (dceExpression i) (dceChunk t) (dceChunk e))
      Lua.Return exp ->
        Just $ node (Lua.Return (dceExpression exp))
      Lua.ForeignSourceCode {} ->
        Just vstat
   where
    node = (Node key scopes,)
    ifKeyIsReachable preserved = do
      vertex <- keyToVertex key
      guard (Set.member vertex reachableVertices) $> preserved

  dceExpression :: Lua.Annotated Node Lua.ExpF -> Lua.Annotated Node Lua.ExpF
  dceExpression indexedExp@(Node key scope, expr) =
    case expr of
      Lua.Nil -> indexedExp
      Lua.Boolean _bool -> indexedExp
      Lua.Integer _int -> indexedExp
      Lua.Float _double -> indexedExp
      Lua.String _text -> indexedExp
      Lua.Function args body ->
        dce (Lua.Function args (dceChunk body))
      Lua.TableCtor rows ->
        dce (Lua.TableCtor (dceTableRow <$> rows))
      Lua.UnOp op e ->
        dce (Lua.UnOp op (dceExpression e))
      Lua.BinOp op e1 e2 ->
        dce (Lua.BinOp op (dceExpression e1) (dceExpression e2))
      Lua.Var v ->
        dce (Lua.Var (dceVar v))
      Lua.FunctionCall e es ->
        dce (Lua.FunctionCall (dceExpression e) (dceExpression <$> es))
   where
    dce = (Node key scope,)

  dceTableRow
    :: Lua.Annotated Node Lua.TableRowF
    -> Lua.Annotated Node Lua.TableRowF
  dceTableRow (Node key scope, row) =
    case row of
      Lua.TableRowKV k v ->
        dce (Lua.TableRowKV (dceExpression k) (dceExpression v))
      Lua.TableRowNV n e ->
        dce (Lua.TableRowNV n (dceExpression e))
   where
    dce = (Node key scope,)

  dceVar :: Lua.Annotated Node Lua.VarF -> Lua.Annotated Node Lua.VarF
  dceVar node@(Node key scope, variable) =
    case variable of
      Lua.VarName _qname ->
        node
      Lua.VarIndex e1 e2 ->
        (Node key scope, Lua.VarIndex (dceExpression e1) (dceExpression e2))
      Lua.VarField e _name ->
        (Node key scope, Lua.VarField (dceExpression e) _name)

  reachableVertices :: Set Vertex
  reachableVertices = Set.fromList $ reachable graph =<< dceEntryVertices

  dceEntryVertices :: [Vertex]
  dceEntryVertices =
    case dceMode of
      PreserveReturned ->
        case viaNonEmpty last statementWithNodes of
          Just (Node k0 _scope0, Lua.Return (Node k1 _scope1, _stat)) ->
            mapMaybe keyToVertex [k0, k1]
          _ -> []
      PreserveTopLevel ->
        mapMaybe (keyToVertex . keyOf . nodeOf) statementWithNodes

adjacencyList :: [Lua.Annotated Node Lua.StatementF] -> [(Label, Key, [Key])]
adjacencyList = DList.toList . (`go` mempty)
 where
  go
    :: [Lua.Annotated Node Lua.StatementF]
    -> DList (Label, Key, [Key])
    -> DList (Label, Key, [Key])
  go [] acc = acc
  go ((Node key _scope, statement) : nextStatements) acc = go nextStatements do
    acc <> case statement of
      Lua.Local name value ->
        DList.cons
          ( "Local(" <> Name.toText name <> ")"
          , key
          , case value of
              Nothing -> findAssignments name nextStatements
              Just (n, _) -> keyOf n : findAssignments name nextStatements
          )
          (maybe mempty expressionAdjacencyList value)
      Lua.Assign variable value ->
        DList.cons
          ("Assign", key, [keyOf (nodeOf variable), keyOf (nodeOf value)])
          (varAdjacencyList variable <> expressionAdjacencyList value)
      Lua.IfThenElse cond th el ->
        DList.cons
          ( "IfThenElse"
          , key
          , keyOf (nodeOf cond)
              : DList.toList (findReturns th <> findReturns el)
          )
          (expressionAdjacencyList cond)
          <> go th mempty
          <> go el mempty
      Lua.Return e ->
        DList.cons
          ("Return", key, [keyOf (nodeOf e)])
          (expressionAdjacencyList e)
      _ -> mempty

findReturns :: [Lua.Annotated Node Lua.StatementF] -> DList Key
findReturns = (keyOf . nodeOf <$>) . findReturnStatements

findReturnStatements
  :: [Lua.Annotated Node Lua.StatementF]
  -> DList (Lua.Annotated Node Lua.StatementF)
findReturnStatements = foldMap \node@(_node, statement) ->
  case statement of
    Lua.Return _ -> DList.singleton node
    Lua.IfThenElse _cond th el ->
      DList.cons node (findReturnStatements th <> findReturnStatements el)
    _ -> DList.empty

findAssignments :: Name -> [Lua.Annotated Node Lua.StatementF] -> [Key]
findAssignments name = foldMap do
  toList
    . (`execAccum` DList.empty)
    . visitStatementM
      makeVisitor
        { beforeStat = \node@(Node key _scope, statement) ->
            case statement of
              Lua.Assign (Lua.Ann (Lua.VarName name')) _val
                | name' == name -> add (DList.singleton key) $> node
              _ -> pure node
        }

expressionAdjacencyList
  :: Lua.Annotated Node Lua.ExpF -> DList (Label, Key, [Key])
expressionAdjacencyList (Node key _scope, expr) =
  case expr of
    Lua.Nil -> pure ("Nil", key, [])
    Lua.Boolean _bool -> pure ("Boolean", key, [])
    Lua.Integer _integer -> pure ("Integer", key, [])
    Lua.Float _double -> pure ("Float", key, [])
    Lua.String _text -> pure ("String", key, [])
    Lua.Function _args body ->
      DList.fromList $
        ("Function", key, DList.toList (findReturns body)) : adjacencyList body
    Lua.TableCtor rows ->
      DList.cons
        ("TableCtor", key, keyOf . nodeOf <$> rows)
        (foldMap rowAdjacencyList rows)
    Lua.UnOp _op e ->
      DList.cons ("UnOp", key, [keyOf (nodeOf e)]) (expressionAdjacencyList e)
    Lua.BinOp _op e1 e2 ->
      DList.cons
        ("BinOp", key, [keyOf (nodeOf e1), keyOf (nodeOf e2)])
        (expressionAdjacencyList e1 <> expressionAdjacencyList e2)
    Lua.Var variable ->
      DList.cons
        ("Var", key, [keyOf (nodeOf variable)])
        (varAdjacencyList variable)
    Lua.FunctionCall e params ->
      DList.cons
        ("FunctionCall", key, keyOf (nodeOf e) : map (keyOf . nodeOf) params)
        (expressionAdjacencyList e <> foldMap expressionAdjacencyList params)

varAdjacencyList :: Lua.Annotated Node Lua.VarF -> DList (Label, Key, [Key])
varAdjacencyList (Node key scopes, variable) =
  case variable of
    Lua.VarName name ->
      DList.singleton
        ( "VarName(Local " <> Name.toText name <> ")"
        , key
        , toList (Map.lookup name (flatten scopes))
        )
    Lua.VarIndex e1 e2 ->
      DList.cons
        ("VarIndex", key, [keyOf (nodeOf e1), keyOf (nodeOf e2)])
        (expressionAdjacencyList e1 <> expressionAdjacencyList e2)
    Lua.VarField e name ->
      DList.cons
        ("VarField(" <> Name.toText name <> ")", key, [keyOf (nodeOf e)])
        (expressionAdjacencyList e)

rowAdjacencyList
  :: Lua.Annotated Node Lua.TableRowF
  -> DList (Label, Key, [Key])
rowAdjacencyList (Node key _scope, row) =
  case row of
    Lua.TableRowKV e1@(n1, _) e2@(n2, _) ->
      DList.cons
        ("Lua.TableRowKV", key, [keyOf n1, keyOf n2])
        (expressionAdjacencyList e1 <> expressionAdjacencyList e2)
    Lua.TableRowNV _name e@(n, _) ->
      DList.cons
        ("Lua.TableRowNV", key, [keyOf n])
        (expressionAdjacencyList e)

invariantViolation :: HasCallStack => Text -> a
invariantViolation msg =
  Prelude.error $
    "The following internal invariant violation happened \
    \while performing a dead-code elimination of the Lua code:\n"
      <> msg

type Label = Text
type Key = Int

--------------------------------------------------------------------------------
-- Annotating statements with graph keys ---------------------------------------

type Scope = Map Name Key

flatten :: [Scope] -> Scope
flatten =
  foldl'
    ( Map.merge Map.preserveMissing Map.preserveMissing $
        Map.zipWithMatched \_name keyInner _keyOuter -> keyInner
    )
    Map.empty

data Node = Node Key [Scope]
  deriving stock (Eq, Show)

keyOf :: Node -> Key
keyOf (Node key _scope) = key

nodeOf :: Lua.Annotated Node f -> Node
nodeOf = fst

makeNodesStatement :: [Lua.Statement] -> [Lua.Annotated Node Lua.StatementF]
makeNodesStatement chunk =
  evalState (forM chunk assignKeys) 0 & \keyedChunk ->
    evalState @[Scope] (assignScopes keyedChunk) []

assignKeys :: Lua.Statement -> State Key (Lua.Annotated Node Lua.StatementF)
assignKeys =
  annotateStatementInsideOutM
    Annotator
      { unAnnotate = Lua.unAnn
      , annotateStat = mkNodeWithKey
      , annotateExp = mkNodeWithKey
      , annotateRow = mkNodeWithKey
      , annotateVar = mkNodeWithKey
      }
    . Lua.ann
 where
  mkNodeWithKey :: f Node -> State Key (Lua.Annotated Node f)
  mkNodeWithKey f = state \key -> ((Node key mempty, f), key + 1)

assignScopes
  :: forall m
   . MonadScopes m
  => [Lua.Annotated Node Lua.StatementF]
  -> m [Lua.Annotated Node Lua.StatementF]
assignScopes = traverse do
  visitStatementM
    makeVisitor
      { beforeStat = beforeStat
      , afterStat = afterStat
      , beforeExp = beforeExp
      , beforeVar = mkNodeWithScopes
      , beforeRow = mkNodeWithScopes
      }
 where
  beforeStat
    :: Lua.Annotated Node Lua.StatementF
    -> m (Lua.Annotated Node Lua.StatementF)
  beforeStat node@(Node key _scopes, stat) =
    case stat of
      Lua.Local name _value -> do
        scopes <- addName name key
        pure (Node key (toList scopes), stat)
      Lua.IfThenElse p t e -> do
        t' <- addScope $> t
        e' <- addScope $> e
        scopes <- getScopes
        pure (Node key (toList scopes), Lua.IfThenElse p t' e')
      _ -> pure node

  afterStat :: Lua.StatementF Node -> m (Lua.StatementF Node)
  afterStat = \case
    stat@Lua.Return {} -> dropScope $> stat
    other -> pure other

  beforeExp :: Lua.Annotated Node Lua.ExpF -> m (Lua.Annotated Node Lua.ExpF)
  beforeExp node@(Node key _scopes, expr) =
    case expr of
      Lua.Function _args _body ->
        addScope <&> \scopes -> (Node key (toList scopes), expr)
      _ -> mkNodeWithScopes node

  mkNodeWithScopes :: (Node, t) -> m (Node, t)
  mkNodeWithScopes (Node key _scopes, t) = getScopes <&> ((,t) . Node key)

unNodesStatement :: Lua.Annotated Node Lua.StatementF -> Lua.Statement
unNodesStatement = unAnnotateStatement Lua.unAnn

class Monad m => MonadScopes m where
  addName :: Name -> Key -> m (NonEmpty Scope)
  addScope :: m (NonEmpty Scope)
  dropScope :: m ()
  getScopes :: m [Scope]

instance Monad m => MonadScopes (StateT [Scope] m) where
  addName name key = do
    scopes <- get
    let scopes' = case scopes of
          [] -> Map.singleton name key :| []
          inner : outer -> Map.insert name key inner :| outer
    put $ toList scopes'
    pure scopes'

  addScope = do
    scopes <- get
    let scopes' = Map.empty :| scopes
    put $ toList scopes'
    pure scopes'

  dropScope = modify' \case
    [] -> []
    _ : remainingScopes -> remainingScopes

  getScopes = get
