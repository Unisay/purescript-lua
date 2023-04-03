{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.PureScript.Backend.Lua.Types where

import Language.PureScript.Backend.Lua.Name (Name)
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Name qualified as Name
import Prettyprinter (Pretty)
import Prelude hiding
  ( and
  , concat
  , error
  , local
  , mod
  , negate
  , or
  , return
  )

type Chunk = [Statement]

data Module = Module
  { moduleChunk :: Chunk
  , moduleName :: ModuleName
  , moduleImports :: [ModuleName]
  , moduleExports :: [Name]
  , moduleForeigns :: [Name]
  , modulePath :: FilePath
  }

newtype ModuleName = ModuleName {unModuleName :: Name}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Pretty)

data QualifiedName = ImportedName ModuleName Name | LocalName Name
  deriving stock (Eq, Ord, Show)

newtype ChunkName = ChunkName Text
  deriving stock (Show)
  deriving newtype (Pretty)

-- | HKD in disguise
type family Annotated (ann :: Type -> Type) (f :: (Type -> Type) -> Type) where
  Annotated Identity f = f Identity
  Annotated ann f = ann (f ann)

data VarF f
  = VarName QualifiedName
  | VarIndex (Annotated f ExpF) (Annotated f ExpF)
  | VarField (Annotated f ExpF) Name

type Var = VarF Identity
deriving stock instance Eq (Annotated f ExpF) => Eq (VarF f)
deriving stock instance Show (Annotated f ExpF) => Show (VarF f)

data TableRowF f
  = TableRowKV (Annotated f ExpF) (Annotated f ExpF)
  | TableRowNV Name (Annotated f ExpF)

type TableRow = TableRowF Identity
deriving stock instance Eq (Annotated f ExpF) => Eq (TableRowF f)
deriving stock instance Show (Annotated f ExpF) => Show (TableRowF f)

data Precedence
  = PrecFunction
  | PrecOperation Natural
  | PrecPrefix
  | PrecAtom
  deriving stock (Show, Eq, Ord)

class HasPrecedence a where
  prec :: a -> Precedence

class HasPrecedence a => HasSymbol a where
  sym :: a -> Text

instance HasPrecedence Precedence where
  prec = id

data UnaryOp = HashOp | Negate | LogicalNot | BitwiseNot
  deriving stock (Show, Eq, Enum, Bounded)

instance HasPrecedence UnaryOp where
  prec =
    PrecOperation . \case
      HashOp -> 11
      Negate -> 11
      LogicalNot -> 11
      BitwiseNot -> 11

instance HasSymbol UnaryOp where
  sym = \case
    HashOp -> "#"
    Negate -> "-"
    LogicalNot -> "not"
    BitwiseNot -> "~"

data BinaryOp
  = Or
  | And
  | LessThan
  | GreaterThan
  | LessThanOrEqualTo
  | GreaterThanOrEqualTo
  | NotEqualTo
  | EqualTo
  | BitOr
  | BitXor
  | BitAnd
  | BitShiftRight
  | BitShiftLeft
  | Concat
  | Add
  | Sub
  | Mul
  | FloatDiv
  | FloorDiv
  | Mod
  | Exp
  deriving stock (Show, Eq, Enum, Bounded)

{- 1   or
   2   and
   3   <     >     <=    >=    ~=    ==
   4   |
   5   ~
   6   &
   7   <<    >>
   8   ..
   9   +     -
   10  *     /     //    %
   11  unary operators (not   #     -     ~)
   12  ^
-}

instance HasPrecedence BinaryOp where
  prec =
    PrecOperation . \case
      Or -> 1
      And -> 2
      LessThan -> 3
      GreaterThan -> 3
      LessThanOrEqualTo -> 3
      GreaterThanOrEqualTo -> 3
      NotEqualTo -> 3
      EqualTo -> 3
      BitOr -> 4
      BitXor -> 5
      BitAnd -> 6
      BitShiftRight -> 7
      BitShiftLeft -> 7
      Concat -> 8
      Add -> 9
      Sub -> 9
      Mul -> 10
      FloatDiv -> 10
      FloorDiv -> 10
      Mod -> 10
      Exp -> 12

instance HasSymbol BinaryOp where
  sym = \case
    Or -> "or"
    And -> "and"
    LessThan -> "<"
    GreaterThan -> ">"
    LessThanOrEqualTo -> "<="
    GreaterThanOrEqualTo -> ">="
    NotEqualTo -> "~="
    EqualTo -> "=="
    BitOr -> "|"
    BitXor -> "~"
    BitAnd -> "&"
    BitShiftRight -> ">>"
    BitShiftLeft -> "<<"
    Concat -> ".."
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    FloatDiv -> "/"
    FloorDiv -> "//"
    Mod -> "%"
    Exp -> "^"

data ExpF (f :: Type -> Type)
  = Nil
  | Boolean Bool
  | Integer Integer
  | Float Double
  | String Text
  | Function [Name] [Annotated f StatementF]
  | TableCtor [Annotated f TableRowF]
  | UnOp UnaryOp (Annotated f ExpF)
  | BinOp BinaryOp (Annotated f ExpF) (Annotated f ExpF)
  | Var (Annotated f VarF)
  | FunctionCall (Annotated f ExpF) [Annotated f ExpF]

type Exp = ExpF Identity
deriving stock instance
  ( Eq (Annotated f VarF)
  , Eq (Annotated f TableRowF)
  , Eq (Annotated f ExpF)
  , Eq (Annotated f StatementF)
  )
  => Eq (ExpF f)
deriving stock instance
  ( Show (Annotated f VarF)
  , Show (Annotated f TableRowF)
  , Show (Annotated f ExpF)
  , Show (Annotated f StatementF)
  )
  => Show (ExpF f)

data StatementF f
  = Assign
      (NonEmpty (Annotated f VarF))
      (NonEmpty (Annotated f ExpF))
  | Local (NonEmpty Name) [Annotated f ExpF]
  | IfThenElse
      (Annotated f ExpF)
      -- ^ predicate
      (NonEmpty (Annotated f StatementF))
      -- ^ then block
      [(Annotated f ExpF, NonEmpty (Annotated f StatementF))]
      -- ^ elseif (predicate, then block)
      (Maybe (NonEmpty (Annotated f StatementF)))
      -- ^ else block
  | Return (Annotated f ExpF)
  | ForeignSourceCode Text

type Statement = StatementF Identity
deriving stock instance
  ( Eq (Annotated f ExpF)
  , Eq (Annotated f VarF)
  , Eq (Annotated f StatementF)
  )
  => Eq (StatementF f)
deriving stock instance
  ( Show (Annotated f ExpF)
  , Show (Annotated f VarF)
  , Show (Annotated f StatementF)
  )
  => Show (StatementF f)

--------------------------------------------------------------------------------
-- Lifted constructors ---------------------------------------------------------

assign1 :: Var -> Exp -> Statement
assign1 v e = Assign (pure v) (pure e)

local1 :: Name -> Exp -> Statement
local1 name expr = Local (pure name) [expr]

thunks :: [Statement] -> Exp
thunks ss = functionCall (Function [] ss) []

-- Expressions -----------------------------------------------------------------

table :: [TableRow] -> Exp
table = TableCtor

varQName :: QualifiedName -> Exp
varQName = Var . VarName

varName :: Name -> Exp
varName = Var . VarName . LocalName

varIndex :: Exp -> Exp -> Exp
varIndex = (Var .) . VarIndex

varField :: Exp -> Name -> Exp
varField = (Var .) . VarField

functionCall :: Exp -> [Exp] -> Exp
functionCall = FunctionCall

require :: ModuleName -> Exp
require (ModuleName modname) =
  functionCall (varName [Lua.name|require|]) [String (Name.toText modname)]

error :: Text -> Exp
error msg = functionCall (varName [Lua.name|error|]) [String msg]

pun :: Name -> TableRow
pun n = TableRowNV n (varName n)

thunk :: Exp -> Exp
thunk e = scope [Return e]

scope :: [Statement] -> Exp
scope body = functionCall (Function [] body) []

-- Unary operators -------------------------------------------------------------

hash :: Exp -> Exp
hash = UnOp HashOp

negate :: Exp -> Exp
negate = UnOp Negate

logicalNot :: Exp -> Exp
logicalNot = UnOp LogicalNot

bitwiseNot :: Exp -> Exp
bitwiseNot = UnOp BitwiseNot

-- Binary operators ------------------------------------------------------------

or :: Exp -> Exp -> Exp
or = BinOp Or

and :: Exp -> Exp -> Exp
and = BinOp And

lessThan :: Exp -> Exp -> Exp
lessThan = BinOp LessThan

greaterThan :: Exp -> Exp -> Exp
greaterThan = BinOp GreaterThan

lessThanOrEqualTo :: Exp -> Exp -> Exp
lessThanOrEqualTo = BinOp LessThanOrEqualTo

greaterThanOrEqualTo :: Exp -> Exp -> Exp
greaterThanOrEqualTo = BinOp GreaterThanOrEqualTo

notEqualTo :: Exp -> Exp -> Exp
notEqualTo = BinOp NotEqualTo

equalTo :: Exp -> Exp -> Exp
equalTo = BinOp EqualTo

bitOr :: Exp -> Exp -> Exp
bitOr = BinOp BitOr

bitXor :: Exp -> Exp -> Exp
bitXor = BinOp BitXor

bitAnd :: Exp -> Exp -> Exp
bitAnd = BinOp BitAnd

bitShiftRight :: Exp -> Exp -> Exp
bitShiftRight = BinOp BitShiftRight

bitShiftLeft :: Exp -> Exp -> Exp
bitShiftLeft = BinOp BitShiftLeft

concat :: Exp -> Exp -> Exp
concat = BinOp Concat

add :: Exp -> Exp -> Exp
add = BinOp Add

sub :: Exp -> Exp -> Exp
sub = BinOp Sub

mul :: Exp -> Exp -> Exp
mul = BinOp Mul

floatDiv :: Exp -> Exp -> Exp
floatDiv = BinOp FloatDiv

floorDiv :: Exp -> Exp -> Exp
floorDiv = BinOp FloorDiv

mod :: Exp -> Exp -> Exp
mod = BinOp Mod

exponent :: Exp -> Exp -> Exp
exponent = BinOp Exp
