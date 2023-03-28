module Language.PureScript.Backend.IR.DCE where

import Data.Graph (Graph, Vertex, graphFromEdges, reachable)
import Data.List (groupBy)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.Types
  ( AbsBinding (..)
  , Argument (..)
  , Binding
  , BindingPattern (findOffset)
  , Exp (..)
  , ExpF (..)
  , Grouping (..)
  , Index (..)
  , Info (..)
  , LetBinding (..)
  , Level (..)
  , Literal (..)
  , LocallyNameless (..)
  , Module (..)
  , ModuleName
  , Name
  , Offset (..)
  , PrimOp (..)
  , Qualified (Imported, Local)
  , bindingNames
  , everywhereTopDownExp
  , listGrouping
  , unLocallyNameless
  , updateRefs
  , wrapExpF
  )
import Relude.Unsafe qualified as Unsafe

data Strategy
  = PreserveSpecified (NonEmpty (ModuleName, NonEmpty Name))
  | PreserveModuleTopLevel (NonEmpty ModuleName)
  | PreserveAllTopLevel
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
      PreserveSpecified points -> toList (toList <<$>> points)
      PreserveAllTopLevel -> (moduleName &&& topLevelNames) <$> modules
      PreserveModuleTopLevel moduleNames ->
        modules >>= \m@Module {moduleName = name} ->
          guard (name `elem` moduleNames) $> (name, topLevelNames m)

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
        <&> dceBinding

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

topLevelNames :: Module -> [Name]
topLevelNames Module {moduleBindings, moduleForeigns} =
  moduleForeigns <> namesFroBindings
 where
  namesFroBindings =
    moduleBindings >>= \case
      Standalone (name, _) -> [name]
      RecursiveGroup bindings -> fst <$> toList bindings

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
  refsFree >>= \case
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

dceBinding :: Binding -> Binding
dceBinding = \case
  Standalone (name, expr) -> Standalone (name, dceExpr expr)
  RecursiveGroup bs -> RecursiveGroup (dceExpr <<$>> bs)

dceExpr :: Exp -> Exp
dceExpr = everywhereTopDownExp \case
  e@(Exp {unExp = Abs (AbsBinding argument body)}) ->
    case argument of
      ArgUnused -> e
      ArgAnonymous -> unusedArg
      ArgNamed _name -> unusedArg
   where
    unusedArg
      | body `refersTo` Index (Level 0) (Offset 0) = e
      | otherwise = wrapExpF $ Abs $ AbsBinding ArgUnused body
  Exp {unExp = Let (LetBinding bindings body)} -> dceLetBinding bindings body
  e -> e

dceLetBinding
  :: NonEmpty (Grouping (Name, LocallyNameless Exp))
  -- ^ Bindings
  -> LocallyNameless Exp
  -- ^ Body
  -> Exp
dceLetBinding (toList -> bindings) body =
  case NE.nonEmpty bindingsToPreserve of
    Nothing -> unLocallyNameless rectifiedBody
    Just someBindings -> wrapExpF (Let (LetBinding someBindings rectifiedBody))
 where
  rectifiedBody = foldr eliminateIndex body indexesToEliminate

  eliminateIndex :: Index -> LocallyNameless Exp -> LocallyNameless Exp
  eliminateIndex eliminatedIndex (LocallyNameless expr) =
    LocallyNameless $
      updateRefs
        (\_level name -> wrapExpF (RefFree (Local name)))
        ( \_level visitedIndex ->
            wrapExpF $
              RefBound
                if offset visitedIndex > offset eliminatedIndex
                  then visitedIndex {offset = offset visitedIndex - 1}
                  else visitedIndex
        )
        expr

  (indexesToEliminate, bindingsToPreserve) =
    partitionEithers $
      bindings >>= \case
        s@(Standalone (name, _expr)) ->
          if any (`refersTo` ix) exprs || body `refersTo` ix
            then [Right s]
            else [Left ix]
         where
          ix = lookupNameIx name
          exprs = fmap snd . listGrouping =<< bindings
        recursiveGroup ->
          if indxs & any \ix ->
            body `refersTo` ix || any (`refersTo` ix) otherBindingsExprs
            then [Right recursiveGroup]
            else Left <$> indxs
         where
          indxs = lookupNameIx <$> bindingNames recursiveGroup
          otherBindingsExprs =
            bindings >>= listGrouping & mapMaybe \(n, e) ->
              if n `elem` bindingNames recursiveGroup
                then Nothing
                else Just e
   where
    lookupNameIx :: Name -> Index
    lookupNameIx name =
      case Index (Level 0) <$> findOffset (bindingNames =<< bindings) name of
        Nothing -> error $ "BUG: " <> show name <> " not found in binding"
        Just index -> index

refersTo :: LocallyNameless Exp -> Index -> Bool
refersTo expr indx = getAny (findIndex indx (unLocallyNameless expr))
 where
  findIndex :: Index -> Exp -> Any
  findIndex index e = case unExp e of
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
    Abs (AbsBinding _arg (LocallyNameless body)) -> lookUnderBinder body
    Let (LetBinding binds (LocallyNameless body)) ->
      foldMap'
        (lookUnderBinder . unLocallyNameless . snd)
        (listGrouping =<< toList binds)
        <> lookUnderBinder body
    IfThenElse p th el -> lookDeeper p <> lookDeeper th <> lookDeeper el
    -- Bottom cases
    RefBound i -> Any (i == index)
    _ -> Any False
   where
    lookDeeper = findIndex index
    lookUnderBinder = findIndex (index {level = level index + 1})
