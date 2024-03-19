module Language.PureScript.Backend.IR.DCE where

import Data.DList (DList)
import Data.DList qualified as DL
import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Types
  ( Annotated
  , Exp
  , Grouping (..)
  , Index
  , Name
  , Parameter (..)
  , QName (..)
  , Qualified (..)
  , RawExp (..)
  , RewriteMod (..)
  , Rewritten (..)
  , annotateExpM
  , groupingNames
  , listGrouping
  , rewriteExpTopDown
  )
import Language.PureScript.Names (ModuleName)

data EntryPoint = EntryPoint ModuleName [Name]
  deriving stock (Show)

-- deriving stock instance Show AExp

eliminateDeadCode ∷ UberModule → UberModule
eliminateDeadCode uber@UberModule {..} =
  -- trace ("\n\nannotatedBindings:\n" <> shower annotatedBindings <> "\n") $
  --   trace ("\nannotatedExports:\n" <> shower annotatedExports <> "\n") $
  --     trace ("\nadjacencyList:\n" <> shower adjacencyList <> "\n") $
  --       trace ("\nreachableIds:\n" <> shower reachableIds <> "\n\n") $
  uber
    { uberModuleBindings = preserveBindings
    , uberModuleExports = preservedExports
    }
 where
  preserveBindings ∷ [Grouping (QName, Exp)]
  preserveBindings = do
    grouping ← annotatedBindings
    case grouping of
      Standalone (nodeId, qname, expr) → do
        -- unless (nodeId `Set.member` reachableIds) $
        --   traceM $ "nodeId " <> shower nodeId <> " not in reachable: " <> shower qname
        guard $ nodeId `Set.member` reachableIds
        [Standalone (qname, dceAnnotatedExp expr)]
      RecursiveGroup recBinds →
        case NE.nonEmpty (preservedRecBinds (toList recBinds)) of
          Nothing → []
          Just pb → [RecursiveGroup pb]
   where
    preservedRecBinds ∷ [(Id, QName, AExp)] → [(QName, Exp)]
    preservedRecBinds recBinds = do
      (nodeId, qname, expr) ← recBinds
      guard $ nodeId `Set.member` reachableIds
      pure (qname, dceAnnotatedExp expr)

  preservedExports ∷ [(Name, Exp)]
  preservedExports = do
    (_id, name, annotatedExp) ← annotatedExports
    pure (name, dceAnnotatedExp annotatedExp)

  annotatedExports ∷ [(Id, Name, AExp)]
  annotatedBindings ∷ [Grouping (Id, QName, AExp)]
  (annotatedExports, annotatedBindings) = runAnnM do
    annExports ← forM uberModuleExports \(name, expr) →
      (,name,) <$> nextId <*> annotateExpWithIds expr
    annBindings ← forM uberModuleBindings $ traverse \(qname, expr) →
      (,qname,) <$> nextId <*> annotateExpWithIds expr
    pure (annExports, annBindings)

  dceAnnotatedExp ∷ AExp → Exp
  dceAnnotatedExp =
    deannotateExp <$> rewriteExpTopDown do
      pure . \case
        Abs (paramId, _param) b
          | not (paramId `Set.member` reachableIds) →
              Rewritten Recurse (Abs (paramId, ParamUnused) b)
        Let binds body@(_bodyId, rawBody) →
          Rewritten Recurse case NE.nonEmpty preservedBinds of
            Nothing → rawBody
            Just bs → Let bs body
         where
          preservedBinds =
            toList binds >>= \case
              Standalone (name, (expId, expr)) → do
                guard $ expId `Set.member` reachableIds
                [Standalone (name, (expId, expr))]
              RecursiveGroup recBinds →
                case NE.nonEmpty preservedRecBinds of
                  Nothing → []
                  Just pb → [RecursiveGroup pb]
               where
                preservedRecBinds =
                  [ b
                  | b@((nameId, _), _) ← toList recBinds
                  , nameId `Set.member` reachableIds
                  ]
        _ → NoChange

  reachableIds ∷ Set Id =
    Set.fromList
      [ nodeId
      | entryVertex ← entryVertices
      , reachableVertex ← reachable graph entryVertex
      , let (_node, nodeId, _deps) = vertexToV reachableVertex
      ]

  entryVertices ∷ [Vertex] =
    [ vtx
    | (nodeId, _name, _exp) ← annotatedExports
    , vtx ← maybeToList (keyToVertex nodeId)
    ]

  ------------------------------------------------------------------------------
  -- Building a graph of nodes -------------------------------------------------

  ( graph ∷ Graph
    , vertexToV ∷ Vertex → ((), Id, [Id])
    , keyToVertex ∷ Id → Maybe Vertex
    ) = graphFromEdges adjacencyList

  adjacencyList ∷ [((), Id, [Id])]
  adjacencyList =
    DL.toList $ adjacencyListFromExports <> adjacencyListFromBindings

  adjacencyListFromExports ∷ DList ((), Id, [Id])
  adjacencyListFromExports =
    annotatedExports & foldMap \(nodeId, _name, expr) →
      adjacencyListFromExport nodeId expr

  adjacencyListFromBindings ∷ DList ((), Id, [Id])
  adjacencyListFromBindings =
    annotatedBindings & foldMap \case
      Standalone (nodeId, _qname, expr) →
        adjacencyListForExpr bindingsInScope (nodeId, expr)
      RecursiveGroup recBinds →
        recBinds & foldMap \(nodeId, _qname, expr) →
          adjacencyListForExpr bindingsInScope (nodeId, expr)

  bindingsInScope ∷ Map (Qualified Name, Index) Id
  bindingsInScope =
    Map.fromList $
      [ ((Imported modname name, 0), bindId)
      | grouping ← annotatedBindings
      , (bindId, QName modname name, _boundExpr) ← listGrouping grouping
      ]

  adjacencyListFromExport ∷ Id → AExp → DList ((), Id, [Id])
  adjacencyListFromExport = curry (adjacencyListForExpr bindingsInScope)

  adjacencyListForExpr
    ∷ Map (Qualified Name, Index) Id
    → (Id, AExp)
    → DList ((), Id, [Id])
  adjacencyListForExpr scope (nodeId, expr) =
    ((), nodeId, expressionDependsOnIds scope expr)
      `DL.cons` case expr of
        LiteralInt {} → mempty
        LiteralFloat {} → mempty
        LiteralString {} → mempty
        LiteralChar {} → mempty
        LiteralBool {} → mempty
        LiteralArray as → foldMap (adjacencyListForExpr scope) as
        LiteralObject ps → foldMap (adjacencyListForExpr scope . snd) ps
        Exception {} → mempty
        ForeignImport {} → mempty
        Ctor {} → mempty
        ReflectCtor a → adjacencyListForExpr scope a
        Eq a b → adjacencyListForExpr scope a <> adjacencyListForExpr scope b
        DataArgumentByIndex _index a → adjacencyListForExpr scope a
        ArrayLength a → adjacencyListForExpr scope a
        ArrayIndex a _index → adjacencyListForExpr scope a
        ObjectProp a _prop → adjacencyListForExpr scope a
        ObjectUpdate o patches →
          adjacencyListForExpr scope o
            <> foldMap (adjacencyListForExpr scope . snd) patches
        IfThenElse i t e →
          adjacencyListForExpr scope i
            <> adjacencyListForExpr scope t
            <> adjacencyListForExpr scope e
        App a b →
          adjacencyListForExpr scope a
            <> adjacencyListForExpr scope b
        Ref _qname _idx →
          mempty
        Abs (_paramId, ParamUnused) b →
          adjacencyListForExpr scope b
        Abs (paramId, ParamNamed param) b →
          DL.cons
            ((), paramId, [])
            (adjacencyListForExpr (addLocalToScope paramId param 0 scope) b)
        Let groupings body →
          adjacencyListForExpr scope' body
            <> snd (foldl' adjacencyListForGrouping (scope, mempty) groupings)
         where
          scope' = foldr addToScope scope (groupingNames =<< toList groupings)
          addToScope (nameId, name) = addLocalToScope nameId name 0
   where
    adjacencyListForGrouping
      ∷ (Map (Qualified Name, Index) Id, DList ((), Id, [Id]))
      → Grouping ((Id, Name), Annotated ((,) Id) RawExp)
      → (Map (Qualified Name, Index) Id, DList ((), Id, [Id]))
    adjacencyListForGrouping (groupingScope, adj) = \case
      Standalone binding@((nameId, _name), (expId, boundExpr)) →
        ( updateScope binding groupingScope
        , DL.cons
            ((), nameId, [expId])
            (adjacencyListForExpr groupingScope (expId, boundExpr) <> adj)
        )
      RecursiveGroup recBinds →
        ( scope'
        , recBinds & foldMap \((nameId, _name), (exprId, boundExpr)) →
            DL.cons
              ((), nameId, [exprId])
              (adjacencyListForExpr scope' (exprId, boundExpr) <> adj)
        )
       where
        scope' = foldr updateScope groupingScope (toList recBinds)
     where
      updateScope
        ∷ ((Id, Name), Annotated ((,) Id) RawExp)
        → Map (Qualified Name, Index) Id
        → Map (Qualified Name, Index) Id
      updateScope ((nameId, name), _) = addLocalToScope nameId name 0

    expressionDependsOnIds ∷ Map (Qualified Name, Index) Id → AExp → [Id]
    expressionDependsOnIds exprScope = \case
      LiteralArray as → fst <$> as
      LiteralObject ps → fst . snd <$> ps
      LiteralInt {} → []
      LiteralFloat {} → []
      LiteralString {} → []
      LiteralChar {} → []
      LiteralBool {} → []
      Exception {} → []
      ForeignImport {} → []
      Ctor {} → []
      ReflectCtor a → [fst a]
      Eq a b → [fst a, fst b]
      DataArgumentByIndex _idx a → [fst a]
      ArrayLength as → [fst as]
      ArrayIndex a _idx → [fst a]
      ObjectProp a _prp → [fst a]
      ObjectUpdate o patches → fst o : toList (fst . snd <$> patches)
      Abs _ b → [fst b]
      App a b → [fst a, fst b]
      IfThenElse i t e → [fst i, fst t, fst e]
      Ref qname idx → maybeToList $ Map.lookup (qname, idx) exprScope
      Let _groupings body → [fst body]

addLocalToScope
  ∷ Id
  → Name
  → Index
  → Map (Qualified Name, Index) Id
  → Map (Qualified Name, Index) Id
addLocalToScope nid name index s =
  let lname = Local name
   in case Map.lookup (lname, index) s of
        Nothing → Map.insert (lname, index) nid s
        Just nid' →
          Map.insert (lname, index) nid $
            addLocalToScope nid' name (succ index) s

--------------------------------------------------------------------------------
-- Annotating expressions with IDs ---------------------------------------------

type Id = Natural

type AExp = RawExp ((,) Id)

newtype AnnM a = AnnM {unAnnM ∷ State Id a}
  deriving newtype (Functor, Applicative, Monad)

nextId ∷ AnnM Id
nextId = AnnM do
  i ← get
  i <$ put (i + 1)

runAnnM ∷ AnnM a → a
runAnnM = (`evalState` 0) . unAnnM

annotateExpWithIds ∷ Exp → AnnM (RawExp ((,) Id))
annotateExpWithIds =
  annotateExpM identity (const nextId) (const nextId) (const nextId)

deannotateExp ∷ AExp → Exp
deannotateExp = \case
  LiteralInt i → LiteralInt i
  LiteralFloat f → LiteralFloat f
  LiteralString s → LiteralString s
  LiteralChar c → LiteralChar c
  LiteralBool b → LiteralBool b
  LiteralArray as → LiteralArray (de <$> as)
  LiteralObject ps → LiteralObject (de <<$>> ps)
  ReflectCtor a → ReflectCtor (de a)
  Eq a b → Eq (de a) (de b)
  DataArgumentByIndex index a → DataArgumentByIndex index (de a)
  ArrayLength a → ArrayLength (de a)
  ArrayIndex a index → ArrayIndex (de a) index
  ObjectProp a prop → ObjectProp (de a) prop
  ObjectUpdate a ps → ObjectUpdate (de a) (de <<$>> ps)
  Abs param body → Abs (pure $ snd param) (de body)
  App a b → App (de a) (de b)
  Ref qname index → Ref qname index
  Let binds body → Let (bimap (pure . snd) de <<$>> binds) (de body)
  IfThenElse i t e → IfThenElse (de i) (de t) (de e)
  Ctor mn aty ty ctor fs → Ctor mn aty ty ctor fs
  Exception m → Exception m
  ForeignImport m p → ForeignImport m p
 where
  de ∷ (a, AExp) → Identity Exp
  de = pure . deannotateExp . snd
