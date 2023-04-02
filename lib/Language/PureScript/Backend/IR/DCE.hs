module Language.PureScript.Backend.IR.DCE where

import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.List (groupBy)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.Types
  ( Binding
  , Exp (..)
  , Grouping (..)
  , Info (..)
  , Module (..)
  , ModuleName
  , Name
  , Qualified (Imported, Local)
  , bindingNames
  )
import Relude.Unsafe qualified as Unsafe

data Strategy
  = EntryPoints (NonEmpty (ModuleName, NonEmpty Name))
  | EntryPointsSomeModules (NonEmpty ModuleName)
  | EntryPointsAllModules
  deriving stock (Show)

eliminateDeadCode :: Strategy -> [Module] -> [Module]
eliminateDeadCode strategy modules = uncurry dceModule <$> reachableByModule
 where
  reachableByModule :: [([(Node, QName, [QName])], Module)]
  reachableByModule = do
    m@Module {moduleName} <- modules
    const m <<$>> filter (snd >>> (== moduleName)) reachableFromEntryPoints

  reachableFromEntryPoints :: [([(Node, QName, [QName])], ModuleName)]
  reachableFromEntryPoints =
    [ (adjacency, modname)
    | (moduleName, names) <- entryPoints
    , vertex <-
        [v | name <- names, v <- maybeToList (keyToVertex (moduleName, name))]
    , adjacency@(_, (modname, _), _) <- vertexToV <$> reachable graph vertex
    ]
      & sortOn snd
      & groupBy ((==) `on` snd)
      & fmap (fmap Unsafe.head . unzip)
   where
    entryPoints :: [(ModuleName, [Name])]
    entryPoints = case strategy of
      EntryPoints points -> toList (toList <<$>> points)
      EntryPointsAllModules -> moduleEntryPoints <$> modules
      EntryPointsSomeModules modulesNames ->
        moduleEntryPoints
          <$> filter (moduleName >>> (`elem` modulesNames)) modules

    moduleEntryPoints :: Module -> (ModuleName, [Name])
    moduleEntryPoints Module {..} =
      (moduleName, moduleForeigns <> (moduleBindings >>= bindingNames))

  (graph, vertexToV, keyToVertex) = buildGraph modules

  dceModule :: [(Node, QName, [QName])] -> Module -> Module
  dceModule adjacencies Module {..} =
    Module
      { moduleName
      , moduleBindings = preserveBindings
      , moduleImports = preservedImports
      , moduleExports = preservedExports
      , moduleReExports = preservedReExports
      , moduleForeigns = preservedForeigns
      , modulePath
      , dataTypes
      }
   where
    preserveBindings :: [Binding]
    preserveBindings =
      filter (any (`Set.member` reachableNames) . bindingNames) moduleBindings
    -- <&> dceBinding

    preservedForeigns :: [Name] =
      filter (`Set.member` reachableNames) moduleForeigns

    preservedImports :: [ModuleName] =
      filter (`Set.member` reachableImports) moduleImports
     where
      reachableImports :: Set ModuleName =
        Set.fromList [m | (_, _, deps) <- adjacencies, (m, _) <- deps]

    preservedExports :: [Name]
    preservedExports = filter (`Set.member` reachableExports) moduleExports
     where
      reachableExports :: Set Name =
        Set.fromList $ preservedForeigns <> (bindingNames =<< preserveBindings)

    preservedReExports :: Map ModuleName [Name] =
      filter (`Set.member` reachableReExports) <$> moduleReExports
     where
      reachableReExports :: Set Name =
        Set.fromList
          [ name
          | -- TODO: filter by the module which re-exports?
          (NodeReExported (_modname, name), _qname, _deps) <- adjacencies
          ]

    reachableNames :: Set Name = Set.fromList $ snd <$> reachableQNames
     where
      reachableQNames :: [QName] =
        [n | (_, qname, deps) <- adjacencies, n <- qname : deps]

type QName = (ModuleName, Name)

data Node
  = NodeBinding Binding
  | NodeForeign QName
  | NodeReExported QName
  deriving stock (Show)

buildGraph
  :: [Module]
  -> ( Graph
     , Vertex -> (Node, QName, [QName])
     , QName -> Maybe Vertex
     )
buildGraph = graphFromEdges . adjacencyList
 where
  -- Builds an adjacency list representing a graph
  -- with vertices of type `QName` labeled by values of type `Node`
  adjacencyList :: [Module] -> [(Node, QName, [QName])]
  adjacencyList modules = do
    m@Module {moduleName = modname} <- modules
    adjacencyListFromDeclarations modname (moduleBindings m)
      <> adjacencyListFromForeigns modname (moduleForeigns m)
      <> adjacencyListFromReExports modname modules (moduleReExports m)

adjacencyListFromDeclarations
  :: ModuleName -> [Binding] -> [(Node, QName, [QName])]
adjacencyListFromDeclarations modname bindings =
  bindings >>= \binding ->
    case binding of
      Standalone (n, e) ->
        [(NodeBinding binding, (modname, n), expDependencies modname e)]
      RecursiveGroup (toList -> binds) -> do
        let as = bimap (modname,) (expDependencies modname) <$> binds
        as <&> \(qn, deps) -> (NodeBinding binding, qn, fmap fst as <> deps)

expDependencies :: ModuleName -> Exp -> [QName]
expDependencies thisModule Exp {expInfo = Info {refsFree}} =
  toList refsFree >>= \case
    Local name -> [(thisModule, name)]
    Imported fromModule name -> [(fromModule, name)]

adjacencyListFromForeigns :: ModuleName -> [Name] -> [(Node, QName, [QName])]
adjacencyListFromForeigns modname = fmap \name ->
  (NodeForeign (modname, name), (modname, name), [])

adjacencyListFromReExports
  :: ModuleName -> [Module] -> Map ModuleName [Name] -> [(Node, QName, [QName])]
adjacencyListFromReExports reexporter modules reExports = do
  m <- modules
  case Map.lookup (moduleName m) reExports of
    Nothing -> []
    Just names -> do
      name <- names
      let qname = (reexporter, name)
      [(NodeReExported qname, qname, [(moduleName m, name)])]

--------------------------------------------------------------------------------
-- Eliminate local subexpressions which are not reachable from the top level ---
--------------------------------------------------------------------------------

{- dceBinding :: Binding -> Binding
dceBinding = \case
  Standalone (name, expr) -> Standalone (name, dceExpr expr)
  RecursiveGroup bs -> RecursiveGroup (dceExpr <<$>> bs)

dceExpr :: Exp -> Exp
dceExpr =
  flip evalState 0 <$> everywhereExpM \case
    e@(Exp {unExp = Abs absBinding}) -> do
      (mbName, body) <- unbindAbs absBinding
      pure case mbName of
        Nothing -> e
        Just name ->
          if body `refersTo` name then e else abstraction ArgUnused body
    Exp {unExp = Let letBinding} -> dceLetBinding letBinding
    e -> pure e
 where
  dceLetBinding :: LetBinding Exp -> State Natural Exp
  dceLetBinding letBinding = do
    (bindings, body) <- unbindLet letBinding
    pure case NE.nonEmpty (bindingsToPreserve (toList bindings) body) of
      Nothing -> body
      Just someBindings -> lets someBindings body
   where
    bindingsToPreserve bindings body =
      bindings >>= \case
        s@(Standalone (name, _expr)) ->
          [s | any (`refersTo` name) exprs || body `refersTo` name]
         where
          exprs = fmap snd . listGrouping =<< bindings
        recursiveGroup ->
          [ recursiveGroup
          | names & any \name ->
              body `refersTo` name || any (`refersTo` name) otherBindingsExprs
          ]
         where
          names = bindingNames recursiveGroup
          otherBindingsExprs =
            bindings >>= listGrouping & mapMaybe \(n, e) ->
              if n `elem` bindingNames recursiveGroup
                then Nothing
                else Just e

refersTo :: Exp -> Name -> Bool
refersTo expr name' = getAny (findName name' expr)
 where
  findName :: Name -> Exp -> Any
  findName name e = case unExp e of
    -- Recursive cases
    Lit (Array as) -> foldMap' lookDeeper as
    Lit (Object props) -> foldMap' (lookDeeper . snd) props
    Prim op ->
      case op of
        ArrayLength a -> lookDeeper a
        ReflectCtor a -> lookDeeper a
        DataArgumentByIndex _idx a -> lookDeeper a
        Eq a b -> lookDeeper a <> lookDeeper b
    ArrayIndex a _indx -> lookDeeper a
    ObjectProp a _prop -> lookDeeper a
    ObjectUpdate a patches ->
      lookDeeper a <> foldMap' (lookDeeper . snd) patches
    App a b -> lookDeeper a <> lookDeeper b
    Abs (AbsBinding _arg body) -> _ body
    Let (LetBinding binds (LocallyNameless body)) ->
      foldMap'
        (lookUnderBinder . unLocallyNameless . snd)
        (listGrouping =<< toList binds)
        <> lookUnderBinder body
    IfThenElse p th el -> lookDeeper p <> lookDeeper th <> lookDeeper el
    -- Bottom cases
    RefFree qname ->
      qualified (Any . (== name)) (const (const (Any False))) qname
    _ -> Any False
   where
    lookDeeper = findName name
 -}
