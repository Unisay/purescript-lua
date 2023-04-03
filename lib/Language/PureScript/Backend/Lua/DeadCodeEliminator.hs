module Language.PureScript.Backend.Lua.DeadCodeEliminator where

import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.List.NonEmpty qualified as NE
import Data.Semialign (rpadZip)
import Language.PureScript.Backend.Lua.Name (Name)
import Language.PureScript.Backend.Lua.Types
import Prelude hiding (exp)

data DceEntry = DceEntryFunctionCall Name | DceEntryLocalDef Name
  deriving stock (Show)

eliminateDeadCode :: NonEmpty DceEntry -> Chunk -> Chunk
eliminateDeadCode entries chunk = chunk >>= eliminateDeadCodeInStatement
 where
  eliminateDeadCodeInStatement :: Statement -> [Statement]
  eliminateDeadCodeInStatement = \case
    Local names vals ->
      case NE.nonEmpty preservedNameValuePairs of
        Nothing -> []
        Just xs -> [Local (fst <$> xs) (mapMaybe snd (toList xs))]
     where
      preservedNameValuePairs =
        filter (isReachableLocalDef . fst) (rpadZip (toList names) vals)
    Return exp ->
      case eliminateDeadExpression exp of
        Nothing -> []
        Just ex -> [Return ex]
    stat -> [stat]

  eliminateDeadExpression :: Exp -> Maybe Exp
  eliminateDeadExpression = \case
    Function args body -> do
      let preservedArgs = filter isReachableFunctionArgument args
      Just $ Function preservedArgs (body >>= eliminateDeadCodeInStatement)
    --   TableCtor rows -> _ -- [TableRow]
    --   UnOp op exp -> _ -- UnaryOp Exp
    --   BinOp op e1 e2 -> _ -- BinaryOp Exp Exp
    --   FunctionCall f args -> _ -- Exp [Exp]
    e -> Just e

  isReachableLocalDef :: Name -> Bool
  isReachableLocalDef =
    flip elem [n | NodeLocalDef n <- reachableVertices]

  isReachableFunctionArgument :: Name -> Bool
  isReachableFunctionArgument =
    flip elem [arg | NodeFunctionArg arg <- reachableVertices]

  reachableVertices :: [Node]
  reachableVertices = do
    dceEntry <- toList entries
    vertex <- dceEntryToVertice dceEntry
    vertexToNode <$> graph `reachable` vertex

  dceEntryToVertice :: DceEntry -> [Vertex]
  dceEntryToVertice entry = case entry of
    DceEntryFunctionCall name ->
      case keyToVertex name of
        Nothing -> invariantViolation ("no vertex found for key " <> show entry)
        Just vx -> [vx]
    DceEntryLocalDef name ->
      case keyToVertex name of
        Nothing -> invariantViolation ("no vertex found for " <> show entry)
        Just vx -> [vx]

  vertexToNode = vertexToAdjacency <&> \(node, _, _) -> node

  ( graph :: Graph
    , vertexToAdjacency :: Vertex -> (Node, Key, [Key])
    , keyToVertex :: Key -> Maybe Vertex
    ) = graphFromEdges adjacencyList

  adjacencyList :: [(Node, Key, [Key])]
  adjacencyList = []

-- adjacencyList = adjacencyListFromStatement =<< chunk

{- adjacencyListFromStatement :: Statement -> [(Node, Key, [Key])]
adjacencyListFromStatement = \case
  Local names vals -> do
    (name, _val) <- rpadZip (toList names) vals
    pure (NodeLocalDef name, name, [])
  Return exp -> adjacencyListFromExpression exp
  _stat -> []

adjacencyListFromExpression :: Exp -> [(Node, Key, [Key])]
adjacencyListFromExpression = \case
  Function args body ->
    [(NodeFunctionArg arg, arg, []) | arg <- args]
      <> (body >>= adjacencyListFromStatement)
  TableCtor rows -> rows >>= adjacencyListFromTableRow
  UnOp {} -> _ -- UnaryOp Exp
  BinOp {} -> _ -- BinaryOp Exp Exp
  Var {} -> _ -- Var
  FunctionCall {} -> _ -- Exp [Exp]
  _terminalExpression -> []

adjacencyListFromTableRow :: TableRow -> [(Node, Key, [Key])]
adjacencyListFromTableRow = \case
  TableRowKV e1 e2 ->
    adjacencyListFromExpression e1 <> adjacencyListFromExpression e2
  TableRowNV n e ->
    (_, _, _) : adjacencyListFromExpression e
  TableRowV {} -> _ -- Exp -}

data Node = NodeLocalDef Name | NodeFunctionArg Name
  deriving stock (Show)

type Key = Name

invariantViolation :: HasCallStack => Text -> a
invariantViolation msg =
  Prelude.error $
    "The following internal invariant violation happened \
    \while performing a dead-code elimination of the Lua code:\n"
      <> msg
