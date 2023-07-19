{-# LANGUAGE QuasiQuotes #-}

module Language.Lua.Types where

import Language.Lua.Name (Name)
import Language.Lua.Name qualified as Lua
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

newtype ChunkName = ChunkName Text
  deriving stock (Show)
  deriving newtype (Pretty)

type Annotated (a ∷ Type) (f ∷ Type → Type) = (a, f a)

pattern Ann ∷ b → (a, b)
pattern Ann fa ← (_ann, fa)
{-# COMPLETE Ann #-}

data ParamF a
  = ParamNamed Name
  | ParamUnused

type Param = ParamF ()

deriving stock instance Eq a ⇒ Eq (ParamF a)
deriving stock instance Ord a ⇒ Ord (ParamF a)
deriving stock instance Show a ⇒ Show (ParamF a)

data VarF a
  = VarName Name
  | VarIndex (Annotated a ExpF) (Annotated a ExpF)
  | VarField (Annotated a ExpF) Name

type Var = VarF ()

deriving stock instance Eq a ⇒ Eq (VarF a)
deriving stock instance Ord a ⇒ Ord (VarF a)
deriving stock instance Show a ⇒ Show (VarF a)

data TableRowF ann
  = TableRowKV (Annotated ann ExpF) (Annotated ann ExpF)
  | TableRowNV Name (Annotated ann ExpF)

type TableRow = TableRowF ()

deriving stock instance Eq a ⇒ Eq (TableRowF a)
deriving stock instance Ord a ⇒ Ord (TableRowF a)
deriving stock instance Show a ⇒ Show (TableRowF a)

data Precedence
  = PrecFunction
  | PrecOperation Natural
  | PrecPrefix
  | PrecTable
  | PrecAtom
  deriving stock (Show, Eq, Ord)

class HasPrecedence a where
  prec ∷ a → Precedence

class HasPrecedence a ⇒ HasSymbol a where
  sym ∷ a → Text

instance HasPrecedence Precedence where
  prec = id

data UnaryOp = HashOp | Negate | LogicalNot | BitwiseNot
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance HasPrecedence UnaryOp where
  prec =
    PrecOperation . \case
      HashOp → 11
      Negate → 11
      LogicalNot → 11
      BitwiseNot → 11

instance HasSymbol UnaryOp where
  sym = \case
    HashOp → "#"
    Negate → "-"
    LogicalNot → "not"
    BitwiseNot → "~"

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
  deriving stock (Show, Eq, Ord, Enum, Bounded)

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
      Or → 1
      And → 2
      LessThan → 3
      GreaterThan → 3
      LessThanOrEqualTo → 3
      GreaterThanOrEqualTo → 3
      NotEqualTo → 3
      EqualTo → 3
      BitOr → 4
      BitXor → 5
      BitAnd → 6
      BitShiftRight → 7
      BitShiftLeft → 7
      Concat → 8
      Add → 9
      Sub → 9
      Mul → 10
      FloatDiv → 10
      FloorDiv → 10
      Mod → 10
      Exp → 12

instance HasSymbol BinaryOp where
  sym = \case
    Or → "or"
    And → "and"
    LessThan → "<"
    GreaterThan → ">"
    LessThanOrEqualTo → "<="
    GreaterThanOrEqualTo → ">="
    NotEqualTo → "~="
    EqualTo → "=="
    BitOr → "|"
    BitXor → "~"
    BitAnd → "&"
    BitShiftRight → ">>"
    BitShiftLeft → "<<"
    Concat → ".."
    Add → "+"
    Sub → "-"
    Mul → "*"
    FloatDiv → "/"
    FloorDiv → "//"
    Mod → "%"
    Exp → "^"

data ExpF ann
  = Nil
  | Boolean Bool
  | Integer Integer
  | Float Double
  | String Text
  | Function [Annotated ann ParamF] [Annotated ann StatementF]
  | TableCtor [Annotated ann TableRowF]
  | UnOp UnaryOp (Annotated ann ExpF)
  | BinOp BinaryOp (Annotated ann ExpF) (Annotated ann ExpF)
  | Var (Annotated ann VarF)
  | FunctionCall (Annotated ann ExpF) [Annotated ann ExpF]

type Exp = ExpF ()

deriving stock instance Eq a ⇒ Eq (ExpF a)
deriving stock instance Ord a ⇒ Ord (ExpF a)
deriving stock instance Show a ⇒ Show (ExpF a)

data StatementF ann
  = Assign (Annotated ann VarF) (Annotated ann ExpF)
  | Local Name (Maybe (Annotated ann ExpF))
  | IfThenElse
      (Annotated ann ExpF)
      -- ^ predicate
      [Annotated ann StatementF]
      -- ^ then block
      [Annotated ann StatementF]
      -- ^ else block
  | Return (Annotated ann ExpF)
  | ForeignSourceCode Text

type Statement = StatementF ()

deriving stock instance Eq a ⇒ Eq (StatementF a)
deriving stock instance Ord a ⇒ Ord (StatementF a)
deriving stock instance Show a ⇒ Show (StatementF a)

--------------------------------------------------------------------------------
-- Smarter constructors --------------------------------------------------------

ann ∷ f () → Annotated () f
ann f = ((), f)

unAnn ∷ Annotated a f → f a
unAnn = snd

var ∷ Var → Exp
var = Var . ann

assign ∷ Var → Exp → Statement
assign v e = Assign (ann v) (ann e)

local ∷ Name → Maybe Exp → Statement
local name expr = Local name (ann <$> expr)

local1 ∷ Name → Exp → Statement
local1 name expr = Local name (Just (ann expr))

local0 ∷ Name → Statement
local0 name = Local name Nothing

ifThenElse ∷ Exp → [Statement] → [Statement] → Statement
ifThenElse i t e = IfThenElse (ann i) (ann <$> t) (ann <$> e)

return ∷ Exp → Statement
return = Return . ann

thunks ∷ [Statement] → Exp
thunks ss = functionCall (Function [] (ann <$> ss)) []

-- Expressions -----------------------------------------------------------------

table ∷ [TableRow] → Exp
table = TableCtor . fmap ann

varName ∷ Name → Exp
varName = Var . ann . VarName

varIndex ∷ Exp → Exp → Exp
varIndex e1 e2 = Var (ann (VarIndex (ann e1) (ann e2)))

varField ∷ Exp → Name → Exp
varField e n = Var (ann (VarField (ann e) n))

functionDef ∷ [Param] → [Statement] → Exp
functionDef params body = Function (ann <$> params) (ann <$> body)

functionCall ∷ Exp → [Exp] → Exp
functionCall f args = FunctionCall (ann f) (ann <$> args)

unOp ∷ UnaryOp → Exp → Exp
unOp op e = UnOp op (ann e)

binOp ∷ BinaryOp → Exp → Exp → Exp
binOp op e1 e2 = BinOp op (ann e1) (ann e2)

error ∷ Text → Exp
error msg = functionCall (varName [Lua.name|error|]) [String msg]

pun ∷ Name → TableRow
pun n = TableRowNV n (ann (varName n))

thunk ∷ Exp → Exp
thunk e = scope [Return (ann e)]

scope ∷ [Statement] → Exp
scope body = functionCall (Function [] (ann <$> body)) []

-- Unary operators -------------------------------------------------------------

hash ∷ Exp → Exp
hash = UnOp HashOp . ann

negate ∷ Exp → Exp
negate = UnOp Negate . ann

logicalNot ∷ Exp → Exp
logicalNot = UnOp LogicalNot . ann

bitwiseNot ∷ Exp → Exp
bitwiseNot = UnOp BitwiseNot . ann

-- Binary operators ------------------------------------------------------------

or ∷ Exp → Exp → Exp
or e1 e2 = BinOp Or (ann e1) (ann e2)

and ∷ Exp → Exp → Exp
and e1 e2 = BinOp And (ann e1) (ann e2)

lessThan ∷ Exp → Exp → Exp
lessThan e1 e2 = BinOp LessThan (ann e1) (ann e2)

greaterThan ∷ Exp → Exp → Exp
greaterThan e1 e2 = BinOp GreaterThan (ann e1) (ann e2)

lessThanOrEqualTo ∷ Exp → Exp → Exp
lessThanOrEqualTo e1 e2 = BinOp LessThanOrEqualTo (ann e1) (ann e2)

greaterThanOrEqualTo ∷ Exp → Exp → Exp
greaterThanOrEqualTo e1 e2 = BinOp GreaterThanOrEqualTo (ann e1) (ann e2)

notEqualTo ∷ Exp → Exp → Exp
notEqualTo e1 e2 = BinOp NotEqualTo (ann e1) (ann e2)

equalTo ∷ Exp → Exp → Exp
equalTo e1 e2 = BinOp EqualTo (ann e1) (ann e2)

bitOr ∷ Exp → Exp → Exp
bitOr e1 e2 = BinOp BitOr (ann e1) (ann e2)

bitXor ∷ Exp → Exp → Exp
bitXor e1 e2 = BinOp BitXor (ann e1) (ann e2)

bitAnd ∷ Exp → Exp → Exp
bitAnd e1 e2 = BinOp BitAnd (ann e1) (ann e2)

bitShiftRight ∷ Exp → Exp → Exp
bitShiftRight e1 e2 = BinOp BitShiftRight (ann e1) (ann e2)

bitShiftLeft ∷ Exp → Exp → Exp
bitShiftLeft e1 e2 = BinOp BitShiftLeft (ann e1) (ann e2)

concat ∷ Exp → Exp → Exp
concat e1 e2 = BinOp Concat (ann e1) (ann e2)

add ∷ Exp → Exp → Exp
add e1 e2 = BinOp Add (ann e1) (ann e2)

sub ∷ Exp → Exp → Exp
sub e1 e2 = BinOp Sub (ann e1) (ann e2)

mul ∷ Exp → Exp → Exp
mul e1 e2 = BinOp Mul (ann e1) (ann e2)

floatDiv ∷ Exp → Exp → Exp
floatDiv e1 e2 = BinOp FloatDiv (ann e1) (ann e2)

floorDiv ∷ Exp → Exp → Exp
floorDiv e1 e2 = BinOp FloorDiv (ann e1) (ann e2)

mod ∷ Exp → Exp → Exp
mod e1 e2 = BinOp Mod (ann e1) (ann e2)

exponent ∷ Exp → Exp → Exp
exponent e1 e2 = BinOp Exp (ann e1) (ann e2)

-- Table Rows ------------------------------------------------------------------

tableRowKV ∷ Exp → Exp → TableRow
tableRowKV k v = TableRowKV (ann k) (ann v)

tableRowNV ∷ Name → Exp → TableRow
tableRowNV n v = TableRowNV n (ann v)
