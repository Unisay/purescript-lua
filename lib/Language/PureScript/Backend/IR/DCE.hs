{-# LANGUAGE LambdaCase #-}

module Language.PureScript.Backend.IR.DCE where

import Control.Monad.Trans.Accum (add, execAccum)
import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Types
  ( Exp (..)
  , ExpF (..)
  , Grouping (..)
  , Info (..)
  , ModuleName
  , Name
  , Parameter (..)
  , QName (..)
  , Qualified (Imported, Local)
  , RewriteMod (..)
  , Rewritten (..)
  , abstraction
  , bindingNames
  , expInfo
  , lets
  , listGrouping
  , rewriteExpTopDown
  , traverseExpM
  )

data EntryPoint = EntryPoint ModuleName [Name]
  deriving stock (Show)

eliminateDeadCode :: EntryPoint -> UberModule -> UberModule
eliminateDeadCode (EntryPoint entryModule entryNames) uber@UberModule {..} =
  uber
    { uberModuleForeigns = preservedForeigns
    , uberModuleBindings = preserveBindings
    }
 where
  preserveBindings :: [Grouping (QName, Exp)]
  preserveBindings =
    [ dceBinding binding
    | binding <- uberModuleBindings
    , any (`Set.member` reachableNames) (bindingNames binding)
    ]

  preservedForeigns :: [(ModuleName, FilePath, NonEmpty Name)] =
    [ (modname, path, qnameName <$> foreignNames)
    | (modname, path, names) <- uberModuleForeigns
    , let qnames = Set.fromList (QName modname <$> toList names)
    , foreignNames <-
        maybeToList $
          NE.nonEmpty (toList (Set.intersection qnames reachableNames))
    ]

  reachableNames :: Set QName =
    Set.fromList
      [ qname
      | fromVertex <- entryVertices
      , reachableVertex <- reachable graph fromVertex
      , let (_node, qname, _deps) = vertexToV reachableVertex
      ]

  entryVertices :: [Vertex] = maybeToList . keyToVertex =<< entryQNames

  entryQNames :: [QName] =
    if null entryNames
      then
        [ qname
        | binding <- uberModuleBindings
        , qname@(QName modname _name) <- bindingNames binding
        , modname == entryModule
        ]
          <> [ QName modname name
             | (modname, _fp, fnames) <- uberModuleForeigns
             , modname == entryModule
             , name <- toList fnames
             ]
      else QName entryModule <$> entryNames

  (graph, vertexToV, keyToVertex) = buildGraph uber

buildGraph
  :: UberModule
  -> ( Graph
     , Vertex -> ((), QName, [QName])
     , QName -> Maybe Vertex
     )
buildGraph = graphFromEdges . adjacencyList
 where
  -- Builds an adjacency list representing a graph
  -- with vertices of type `QName` labeled by values of type `Node`
  adjacencyList :: UberModule -> [((), QName, [QName])]
  adjacencyList uberModule =
    adjacencyListFromBindings (uberModuleBindings uberModule)
      <> [ ((), qname, [])
         | (modname, _filePath, names) <- uberModuleForeigns uberModule
         , name <- toList names
         , let qname = QName modname name
         ]

adjacencyListFromBindings
  :: [Grouping (QName, Exp)] -> [((), QName, [QName])]
adjacencyListFromBindings bindings =
  bindings >>= \case
    Standalone (qname, e) ->
      [((), qname, expDependencies (qnameModuleName qname) e)]
    RecursiveGroup (toList -> binds) ->
      bindsDeps <&> \(qname, deps) ->
        ((), qname, [qn | (qn, _deps) <- bindsDeps, qname /= qn] <> deps)
     where
      bindsDeps :: [(QName, [QName])] =
        binds <&> \(qname@QName {qnameModuleName}, expr) ->
          (qname, expDependencies qnameModuleName expr)

expDependencies :: ModuleName -> Exp -> [QName]
expDependencies thisModule expr =
  Map.keys (refsFree (expInfo expr)) >>= \case
    Local name -> [QName thisModule name]
    Imported fromModule name -> [QName fromModule name]

--------------------------------------------------------------------------------
-- Eliminate local subexpressions which are not reachable from the top level ---
--------------------------------------------------------------------------------

dceBinding :: Grouping (QName, Exp) -> Grouping (QName, Exp)
dceBinding = \case
  Standalone (qname, expr) -> Standalone (qname, dceExpr expr)
  RecursiveGroup bs -> RecursiveGroup (dceExpr <<$>> bs)

dceExpr :: Exp -> Exp
dceExpr =
  rewriteExpTopDown $
    pure . \case
      Exp {unExp = App (unExp -> Abs ParamUnused body) _} ->
        Rewritten Recurse body
      Exp {unExp = Abs argument body} ->
        case argument of
          ParamUnused -> NoChange
          ParamNamed name ->
            if body `refersTo` name
              then NoChange
              else Rewritten Recurse (abstraction ParamUnused body)
      Exp {unExp = Let binds body} -> dceLetBinding binds body
      _ -> NoChange
 where
  dceLetBinding :: NonEmpty (Grouping (Name, Exp)) -> Exp -> Rewritten Exp
  dceLetBinding bindings body =
    if length originalBindings == length preservedBindings
      then NoChange
      else Rewritten Recurse case NE.nonEmpty preservedBindings of
        Nothing -> body
        Just someBindings -> lets someBindings body
   where
    originalBindings = toList bindings
    preservedBindings =
      originalBindings >>= \case
        s@(Standalone (name, _expr)) ->
          [s | any (`refersTo` name) exprs || body `refersTo` name]
         where
          exprs = fmap snd . listGrouping =<< originalBindings
        recursiveGroup ->
          [ recursiveGroup
          | names & any \name ->
              body `refersTo` name || any (`refersTo` name) otherBindingsExprs
          ]
         where
          names = bindingNames recursiveGroup
          otherBindingsExprs =
            originalBindings >>= listGrouping & mapMaybe \(n, e) ->
              if n `elem` bindingNames recursiveGroup
                then Nothing
                else Just e

refersTo :: Exp -> Name -> Bool
refersTo e name =
  getAny (execAccum (traverseExpM (visit . unExp) identity e) (Any False))
 where
  visit = \case
    Ref (Local refName) _index | refName == name -> add $ Any True
    _ -> pass
