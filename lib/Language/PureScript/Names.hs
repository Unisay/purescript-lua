{-# LANGUAGE TemplateHaskell #-}

-- | Data types for names
module Language.PureScript.Names where

import Data.Aeson
  ( FromJSON (parseJSON)
  , FromJSONKey (fromJSONKey)
  , Options (sumEncoding)
  , SumEncoding (ObjectWithSingleField)
  , ToJSON (toJSON)
  , ToJSONKey (toJSONKey)
  , defaultOptions
  , parseJSON2
  , toJSON2
  , withArray
  )
import Data.Aeson.TH (deriveJSON)
import Data.Text qualified as T
import Data.Vector qualified as V

-- | A sum of the possible name types, useful for error and lint messages.
data Name
  = IdentName Ident
  | TyName (ProperName 'TypeName)
  | DctorName (ProperName 'ConstructorName)
  | TyClassName (ProperName 'ClassName)
  | ModName ModuleName
  deriving stock (Eq, Ord, Show, Generic)

getIdentName :: Name -> Maybe Ident
getIdentName (IdentName name) = Just name
getIdentName _ = Nothing

getTypeName :: Name -> Maybe (ProperName 'TypeName)
getTypeName (TyName name) = Just name
getTypeName _ = Nothing

getDctorName :: Name -> Maybe (ProperName 'ConstructorName)
getDctorName (DctorName name) = Just name
getDctorName _ = Nothing

getClassName :: Name -> Maybe (ProperName 'ClassName)
getClassName (TyClassName name) = Just name
getClassName _ = Nothing

{- |
This type is meant to be extended with any new uses for idents that come
along. Adding constructors to this type is cheaper than adding them to
`Ident` because functions that match on `Ident` can ignore all
`InternalIdent`s with a single pattern, and thus don't have to change if
a new `InternalIdentData` constructor is created.
-}
data InternalIdentData
  = -- Used by CoreFn.Laziness
    RuntimeLazyFactory
  | Lazy !Text
  deriving stock (Show, Eq, Ord, Generic)

-- | Names for value identifiers
data Ident
  = -- |
    -- An alphanumeric identifier
    Ident Text
  | -- |
    -- A generated name for an identifier
    GenIdent (Maybe Text) Integer
  | -- |
    -- A generated name used only for type-checking
    UnusedIdent
  | -- |
    -- A generated name used only for internal transformations
    InternalIdent !InternalIdentData
  deriving stock (Show, Eq, Ord, Generic)

runIdent :: Ident -> Text
runIdent = \case
  Ident i -> i
  GenIdent Nothing n -> "$" <> show n
  GenIdent (Just name) n -> "$" <> name <> show n
  UnusedIdent -> unusedIdent
  InternalIdent internalIdentData ->
    case internalIdentData of
      RuntimeLazyFactory -> "$__runtime_lazy"
      Lazy t -> "$__lazy_" <> t

unusedIdent :: Text
unusedIdent = "$__unused"

{- | Proper names, i.e. capitalized names for e.g. module names,
type/data constructors.
-}
newtype ProperName (a :: ProperNameType) = ProperName {runProperName :: Text}
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON (ProperName a) where
  toJSON = toJSON . runProperName

instance FromJSON (ProperName a) where
  parseJSON = fmap ProperName . parseJSON

-- | The closed set of proper name types.
data ProperNameType
  = TypeName
  | ConstructorName
  | ClassName
  | Namespace

{- |
Coerces a ProperName from one ProperNameType to another. This should be used
with care, and is primarily used to convert ClassNames into TypeNames after
classes have been desugared.
-}
coerceProperName :: ProperName a -> ProperName b
coerceProperName = ProperName . runProperName

-- | Module names
newtype ModuleName = ModuleName Text
  deriving stock (Show, Eq, Ord, Generic)

runModuleName :: ModuleName -> Text
runModuleName (ModuleName name) = name

moduleNameFromString :: Text -> ModuleName
moduleNameFromString = ModuleName

isBuiltinModuleName :: ModuleName -> Bool
isBuiltinModuleName (ModuleName mn) = mn == "Prim" || "Prim." `T.isPrefixOf` mn

-- | Source position information
data SourcePos = SourcePos
  { sourcePosLine :: Int
  , sourcePosColumn :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

displaySourcePos :: SourcePos -> Text
displaySourcePos sp =
  "line " <> show (sourcePosLine sp) <> ", column " <> show (sourcePosColumn sp)

displaySourcePosShort :: SourcePos -> Text
displaySourcePosShort sp =
  show (sourcePosLine sp) <> ":" <> show (sourcePosColumn sp)

instance ToJSON SourcePos where
  toJSON SourcePos {..} =
    toJSON [sourcePosLine, sourcePosColumn]

instance FromJSON SourcePos where
  parseJSON arr = do
    [line, col] <- parseJSON arr
    return $ SourcePos line col

data QualifiedBy
  = BySourcePos SourcePos
  | ByModuleName ModuleName
  deriving stock (Show, Eq, Ord, Generic)

pattern ByNullSourcePos :: QualifiedBy
pattern ByNullSourcePos = BySourcePos (SourcePos 0 0)

isBySourcePos :: QualifiedBy -> Bool
isBySourcePos (BySourcePos _) = True
isBySourcePos _ = False

byMaybeModuleName :: Maybe ModuleName -> QualifiedBy
byMaybeModuleName (Just mn) = ByModuleName mn
byMaybeModuleName Nothing = ByNullSourcePos

toMaybeModuleName :: QualifiedBy -> Maybe ModuleName
toMaybeModuleName (ByModuleName mn) = Just mn
toMaybeModuleName (BySourcePos _) = Nothing

{- |
A qualified name, i.e. a name with an optional module name
-}
data Qualified a = Qualified QualifiedBy a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

showQualified :: (a -> Text) -> Qualified a -> Text
showQualified f = \case
  Qualified (BySourcePos _) a -> f a
  Qualified (ByModuleName name) a -> runModuleName name <> "." <> f a

getQual :: Qualified a -> Maybe ModuleName
getQual (Qualified qb _) = toMaybeModuleName qb

{- |
Provide a default module name, if a name is unqualified
-}
qualify :: ModuleName -> Qualified a -> (ModuleName, a)
qualify m (Qualified (BySourcePos _) a) = (m, a)
qualify _ (Qualified (ByModuleName m) a) = (m, a)

{- |
Makes a qualified value from a name and module name.
-}
mkQualified :: a -> ModuleName -> Qualified a
mkQualified name mn = Qualified (ByModuleName mn) name

-- | Remove the module name from a qualified name
disqualify :: Qualified a -> a
disqualify (Qualified _ a) = a

{- |
Remove the qualification from a value when it is qualified with a particular
module name.
-}
disqualifyFor :: Maybe ModuleName -> Qualified a -> Maybe a
disqualifyFor mn (Qualified qb a) | mn == toMaybeModuleName qb = Just a
disqualifyFor _ _ = Nothing

{- |
Checks whether a qualified value is actually qualified with a module reference
-}
isQualified :: Qualified a -> Bool
isQualified (Qualified (BySourcePos _) _) = False
isQualified _ = True

{- |
Checks whether a qualified value is not actually qualified with a module reference
-}
isUnqualified :: Qualified a -> Bool
isUnqualified = not . isQualified

{- |
Checks whether a qualified value is qualified with a particular module
-}
isQualifiedWith :: ModuleName -> Qualified a -> Bool
isQualifiedWith mn (Qualified (ByModuleName mn') _) = mn == mn'
isQualifiedWith _ _ = False

instance ToJSON a => ToJSON (Qualified a) where
  toJSON (Qualified qb a) = case qb of
    ByModuleName mn -> toJSON2 (mn, a)
    BySourcePos ss -> toJSON2 (ss, a)

instance FromJSON a => FromJSON (Qualified a) where
  parseJSON v = byModule <|> bySourcePos <|> byMaybeModuleName'
   where
    byModule = do
      (mn, a) <- parseJSON2 v
      pure $ Qualified (ByModuleName mn) a
    bySourcePos = do
      (ss, a) <- parseJSON2 v
      pure $ Qualified (BySourcePos ss) a
    byMaybeModuleName' = do
      (mn, a) <- parseJSON2 v
      pure $ Qualified (byMaybeModuleName mn) a

instance ToJSON ModuleName where
  toJSON (ModuleName name) = toJSON (T.splitOn "." name)

instance FromJSON ModuleName where
  parseJSON = withArray "ModuleName" $ \names -> do
    names' <- traverse parseJSON names
    pure (ModuleName (T.intercalate "." (V.toList names')))

instance ToJSONKey ModuleName where
  toJSONKey = contramap runModuleName toJSONKey

instance FromJSONKey ModuleName where
  fromJSONKey = fmap moduleNameFromString fromJSONKey

$(deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField}) ''InternalIdentData)
$(deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField}) ''Ident)
