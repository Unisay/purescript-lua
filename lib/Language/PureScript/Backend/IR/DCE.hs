module Language.PureScript.Backend.IR.DCE
  ( EntryPoint (..)
  , eliminateDeadCode
  ) where

import Data.DList (DList)
import Data.DList qualified as DL
import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set (member)
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Names
  ( ModuleName
  , Name (..)
  , QName (..)
  , Qualified (..)
  )
import Language.PureScript.Backend.IR.Types
  ( Ann
  , Exp
  , Grouping (..)
  , Index
  , Parameter (..)
  , RawExp (..)
  , RewriteMod (..)
  , Rewritten (..)
  , annotateExpM
  , getAnn
  , listGrouping
  , rewriteExpTopDown
  )

data EntryPoint = EntryPoint ModuleName [Name]
  deriving stock (Show)

type Scope = Map (Qualified Name, Index) Id

type Node = ((), Id, [Id])

eliminateDeadCode ∷ UberModule → UberModule
eliminateDeadCode uber@UberModule {..} =
  -- traceIt "annotatedForeigns" annotatedForeigns $
  --   traceIt "annotatedBindings" annotatedBindings $
  --     traceIt "annotatedExports" annotatedExports $
  --       traceIt "topLevelScope" topLevelScope $
  --         traceIt "adjacencyList" adjacencyList $
  --           traceIt "reachableIds" reachableIds $
  uber
    { uberModuleForeigns = preservedForeigns
    , uberModuleBindings = preservedBindings
    , uberModuleExports = preservedExports
    }
 where
  -- traceIt ∷ ∀ a b. Show a ⇒ String → a → b → b
  -- traceIt label it =
  --   trace ("\n\n" <> label <> ":\n" <> pp it <> "\n")
  --  where
  --   pp =
  --     toString
  --       . pShowOpt
  --         defaultOutputOptionsDarkBg
  --           { outputOptionsCompact = True
  --           }

  preservedForeigns ∷ [(QName, Exp)]
  preservedForeigns = do
    (name, expr) ← annotatedForeigns
    guard $ nodeId expr `member` reachableIds
    pure . (name,) $ case expr of
      ForeignImport (_id, ann) modname path names →
        ForeignImport
          ann
          modname
          path
          [(a, n) | ((i, a), n) ← names, i `member` reachableIds]
      other → dceAnnotatedExp other

  preservedBindings ∷ [Grouping (QName, Exp)] =
    annotatedBindings >>= \case
      Standalone (qname, expr) → do
        guard $ nodeId expr `member` reachableIds
        [Standalone (qname, dceAnnotatedExp expr)]
      RecursiveGroup recBinds →
        case NE.nonEmpty (preservedRecBinds (toList recBinds)) of
          Nothing → []
          Just pb → [RecursiveGroup pb]
   where
    preservedRecBinds ∷ [(QName, AExp)] → [(QName, Exp)]
    preservedRecBinds recBinds = do
      (qname, expr) ← recBinds
      guard $ nodeId expr `member` reachableIds
      pure (qname, dceAnnotatedExp expr)

  preservedExports ∷ [(Name, Exp)]
  preservedExports = do
    (name, annotatedExp) ← annotatedExports
    pure (name, dceAnnotatedExp annotatedExp)

  -- run these computations in the same monad
  -- so that we can share the state of the ID counter
  ( annotatedForeigns ∷ [(QName, AExp)]
    , annotatedBindings ∷ [Grouping (QName, AExp)]
    , annotatedExports ∷ [(Name, AExp)]
    ) = runAnnM do
      liftA3
        (,,)
        (traverse (traverse assignUniqueIds) uberModuleForeigns)
        (traverse (traverse (traverse assignUniqueIds)) uberModuleBindings)
        (traverse (traverse assignUniqueIds) uberModuleExports)

  annotatedForeignImports ∷ [(QName, AExp)] =
    [i | i@(_qname, ForeignImport {}) ← annotatedForeigns]

  annotatedForeignBindings ∷ [(QName, AExp)] =
    [b | b@(_qname, ObjectProp {}) ← annotatedForeigns]

  dceAnnotatedExp ∷ AExp → Exp
  dceAnnotatedExp =
    deannotateExp <$> rewriteExpTopDown do
      pure . \case
        Abs ann param b
          | not (paramId `member` reachableIds) →
              Rewritten Recurse (Abs ann param' b)
         where
          paramId ∷ Id =
            case param of
              ParamUnused (pid, _) → pid
              ParamNamed (pid, _) _name → pid
          param' =
            case param of
              ParamUnused pann → ParamUnused pann
              ParamNamed pann _name → ParamUnused pann
        Let ann binds body →
          Rewritten Recurse case NE.nonEmpty preservedBinds of
            Nothing → body
            Just bs → Let ann bs body
         where
          preservedBinds ∷ [Grouping ((Id, Ann), Name, AExp)]
          preservedBinds =
            toList binds >>= \case
              b@(Standalone ((expId, _ann), _name, _expr)) →
                [b | expId `member` reachableIds]
              RecursiveGroup recBinds →
                case NE.nonEmpty preservedRecBinds of
                  Nothing → []
                  Just pb → [RecursiveGroup pb]
               where
                preservedRecBinds =
                  [ b
                  | b@((nameId, _ann), _, _) ← toList recBinds
                  , nameId `member` reachableIds
                  ]
        _ → NoChange

  reachableIds ∷ Set Id =
    Set.fromList
      [ node
      | entryVertex ← entryVertices
      , reachableVertex ← reachable graph entryVertex
      , let (_node, node, _deps) = vertexToV reachableVertex
      ]

  entryVertices ∷ [Vertex] =
    [ vtx
    | (_name, expr) ← annotatedExports
    , vtx ← maybeToList (keyToVertex (nodeId expr))
    ]

  ------------------------------------------------------------------------------
  -- Building a graph of nodes -------------------------------------------------

  ( graph ∷ Graph
    , vertexToV ∷ Vertex → Node
    , keyToVertex ∷ Id → Maybe Vertex
    ) = graphFromEdges (toList adjacencyList)

  -- Crash if the adjacency list is not complete:
  -- every referenced node must be present in the list.
  -- assertAdjacencyListIsComplete
  --   ∷ HasCallStack
  --   ⇒ [Node]
  --   → [Node]
  -- assertAdjacencyListIsComplete al =
  --   if referencedNodes `isSubsetOf` nodes
  --     then al
  --     else
  --       error . unlines $
  --         [ "Incomplete adjacency list: "
  --         , toText (pShow al)
  --         , "Nodes: " <> toText (pShow nodes)
  --         , "Referenced nodes: " <> toText (pShow referencedNodes)
  --         ]
  --  where
  --   nodes = Set.fromList (al <&> \((), node, _) → node)
  --   referencedNodes = Set.fromList (al >>= \((), _, refs) → refs)

  mkNode ∷ Id → [Id] → Node
  mkNode = ((),,)

  adjacencyList ∷ DList Node =
    adjacencyListFromForeignImports
      <> adjacencyListFromForeignBindings
      <> adjacencyListFromExports
      <> adjacencyListFromBindings

  adjacencyListFromForeignImports ∷ DList Node = DL.fromList do
    annotatedForeignImports <&> \(_qname, expr) → mkNode (nodeId expr) []

  -- The functionality which builds adjacency list for foreign bindings
  -- depends on the particular sturcture emitted by the 'Linker' and therefore
  -- is not generic.
  adjacencyListFromForeignBindings ∷ DList Node =
    annotatedForeignBindings & foldMap \case
      ( QName bindingModule bindingName
        , ObjectProp (objPropId, _) (Ref (objRefId, _) _ 0) _prop
        ) →
          DL.fromList do
            mkNode objPropId (objRefId : map fst foreignImportForBinding)
              : mkNode objRefId (map snd foreignImportForBinding)
              : [mkNode propId [] | (propId, _) ← foreignImportForBinding]
         where
          foreignImportForBinding ∷ [(Id, Id)] =
            [ (propId, importId)
            | ( QName importModule _foreign
                , ForeignImport (importId, _) _ _ propNames
                ) ←
                annotatedForeignImports
            , bindingModule == importModule
            , ((propId, _ann), propName) ← propNames
            , propName == bindingName
            ]
      _ → DL.empty

  adjacencyListFromExports ∷ DList Node =
    annotatedExports & foldMap \(_name, expr) →
      adjacencyListForExpr topLevelScope expr

  adjacencyListFromBindings ∷ DList Node =
    annotatedBindings & foldMap \case
      Standalone (_qname, expr) →
        adjacencyListForExpr topLevelScope expr
      RecursiveGroup recBinds →
        recBinds & foldMap \(_qname, expr) →
          adjacencyListForExpr topLevelScope expr

  topLevelScope ∷ Scope =
    Map.fromList (foreignsInScope <> bindingsInScope)
   where
    foreignsInScope = do
      (QName modname name, expr) ← annotatedForeigns
      pure ((Imported modname name, 0), nodeId expr)

    bindingsInScope = do
      (QName modname name, expr) ← listGrouping =<< annotatedBindings
      pure ((Imported modname name, 0), nodeId expr)

  adjacencyListForExpr ∷ Scope → AExp → DList Node
  adjacencyListForExpr scope expr =
    mkNode (nodeId expr) (expressionDependsOnIds scope expr)
      `DL.cons` case expr of
        LiteralInt {} → mempty
        LiteralFloat {} → mempty
        LiteralString {} → mempty
        LiteralChar {} → mempty
        LiteralBool {} → mempty
        LiteralArray _ann as → foldMap (adjacencyListForExpr scope) as
        LiteralObject _ann ps → foldMap (adjacencyListForExpr scope . snd) ps
        Exception {} → mempty
        ForeignImport {} → mempty
        Ctor {} → mempty
        ReflectCtor _ann a →
          adjacencyListForExpr scope a
        Eq _ann a b →
          adjacencyListForExpr scope a <> adjacencyListForExpr scope b
        DataArgumentByIndex _ann _index a →
          adjacencyListForExpr scope a
        ArrayLength _ann a →
          adjacencyListForExpr scope a
        ArrayIndex _ann a _index →
          adjacencyListForExpr scope a
        ObjectProp _ann a _prop →
          adjacencyListForExpr scope a
        ObjectUpdate _ann o patches →
          adjacencyListForExpr scope o
            <> foldMap (adjacencyListForExpr scope . snd) patches
        IfThenElse _ann i t e →
          adjacencyListForExpr scope i
            <> adjacencyListForExpr scope t
            <> adjacencyListForExpr scope e
        App _ann a b →
          adjacencyListForExpr scope a <> adjacencyListForExpr scope b
        Ref _ann _qname _idx →
          mempty
        Abs _ann param b →
          case param of
            ParamUnused _ann' → adjacencyListForExpr scope b
            ParamNamed (paramId, _ann) name →
              DL.cons
                (mkNode paramId [])
                (adjacencyListForExpr (addLocalToScope paramId name 0 scope) b)
        Let _ann groupings body →
          adjacencyListForExpr scope' body
            <> snd (foldl' adjacencyListForGrouping (scope, mempty) groupings)
         where
          scope' = foldr addToScope scope (listGrouping =<< toList groupings)
          addToScope ∷ ((Id, ann), Name, expr) → Scope → Scope
          addToScope ((nameId, _ann), name, _expr) =
            addLocalToScope nameId name 0
   where
    adjacencyListForGrouping
      ∷ (Scope, DList Node)
      → Grouping ((Id, Ann), Name, AExp)
      → (Scope, DList Node)
    adjacencyListForGrouping (groupingScope, adj) = \case
      Standalone binding@((nameId, _ann), _name, boundExpr) →
        ( updateScope binding groupingScope
        , DL.cons
            (mkNode nameId [nodeId boundExpr])
            (adjacencyListForExpr groupingScope boundExpr <> adj)
        )
      RecursiveGroup recBinds →
        ( scope'
        , recBinds & foldMap \((nameId, _ann), _name, boundExpr) →
            DL.cons
              (mkNode nameId [nodeId boundExpr])
              (adjacencyListForExpr scope' boundExpr <> adj)
        )
       where
        scope' = foldr updateScope groupingScope (toList recBinds)
     where
      updateScope ∷ ((Id, Ann), Name, AExp) → Scope → Scope
      updateScope ((nameId, _ann), name, _expr) = addLocalToScope nameId name 0

expressionDependsOnIds ∷ Scope → AExp → [Id]
expressionDependsOnIds exprScope = \case
  LiteralArray _ann as → nodeId <$> as
  LiteralObject _ann ps → nodeId . snd <$> ps
  LiteralInt {} → []
  LiteralFloat {} → []
  LiteralString {} → []
  LiteralChar {} → []
  LiteralBool {} → []
  Exception {} → []
  ForeignImport {} → []
  Ctor {} → []
  ReflectCtor _ann a → [nodeId a]
  Eq _ann a b → [nodeId a, nodeId b]
  DataArgumentByIndex _ann _idx a → [nodeId a]
  ArrayLength _ann as → [nodeId as]
  ArrayIndex _ann a _idx → [nodeId a]
  ObjectProp _ann a _prp → [nodeId a]
  ObjectUpdate _ann o patches → nodeId o : toList (nodeId . snd <$> patches)
  Abs _ann _ b → [nodeId b]
  App _ann a b → [nodeId a, nodeId b]
  IfThenElse _ann i t e → [nodeId i, nodeId t, nodeId e]
  Ref _ann qname idx → maybeToList $ Map.lookup (qname, idx) exprScope
  Let _ann _groupings body → [nodeId body]

addLocalToScope ∷ Id → Name → Index → Scope → Scope
addLocalToScope nid name index s =
  case Map.lookup (lname, index) s of
    Nothing → Map.insert (lname, index) nid s
    Just nid' →
      Map.insert (lname, index) nid $
        addLocalToScope nid' name (succ index) s
 where
  lname = Local name

--------------------------------------------------------------------------------
-- Annotating expressions with IDs ---------------------------------------------

type Id = Natural

type AExp = RawExp (Id, Ann)

newtype AnnM a = AnnM {unAnnM ∷ State Id a}
  deriving newtype (Functor, Applicative, Monad)

nodeId ∷ AExp → Id
nodeId = fst . getAnn

nextId ∷ AnnM Id
nextId = AnnM (get >>= \i → i <$ put (i + 1))

runAnnM ∷ AnnM a → a
runAnnM = (`evalState` 0) . unAnnM

assignUniqueIds ∷ Exp → AnnM AExp
assignUniqueIds =
  annotateExpM identity annotateExp annotateParam annotateName
 where
  annotateExp ∷ RawExp Ann → AnnM (Id, Ann)
  annotateExp e = (,getAnn e) <$> nextId

  annotateParam ∷ Parameter Ann → AnnM (Parameter (Id, Ann))
  annotateParam = \case
    ParamNamed ann name → do
      id' ← nextId
      pure $ ParamNamed (id', ann) name
    ParamUnused ann → do
      id' ← nextId
      pure $ ParamUnused (id', ann)

  annotateName ∷ Ann → Name → AnnM (Id, Ann)
  annotateName a _name = (,a) <$> nextId

deannotateExp ∷ AExp → Exp
deannotateExp expr = runIdentity do
  annotateExpM identity deannotateExp' deannotateParam deannotateName expr
 where
  deannotateExp' ∷ Applicative f ⇒ RawExp (Id, Ann) → f Ann
  deannotateExp' = pure . snd . getAnn

  deannotateParam ∷ Applicative f ⇒ Parameter (Id, Ann) → f (Parameter Ann)
  deannotateParam = \case
    ParamNamed (_id, ann) name → pure $ ParamNamed ann name
    ParamUnused (_id, ann) → pure $ ParamUnused ann

  deannotateName ∷ Applicative f ⇒ (Id, Ann) → Name → f Ann
  deannotateName (_id, ann) _name = pure ann
