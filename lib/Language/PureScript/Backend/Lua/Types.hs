{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.Types where

import Control.Lens (Lens', lens)
import Data.DList (DList)
import Language.PureScript.Backend.Lua.Name (Name)
import Language.PureScript.Backend.Lua.Name qualified as Lua
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

type Chunk = DList Statement

newtype ChunkName = ChunkName Text
  deriving stock (Show)
  deriving newtype (Pretty)

{- pattern Ann ∷ b → (a, b)
pattern Ann fa ← (_ann, fa)
{-# COMPLETE Ann #-} -}

data ParamF ann
  = ParamNamed ann Name
  | ParamUnused ann

type Param = ParamF Ann

deriving stock instance Eq a ⇒ Eq (ParamF a)
deriving stock instance Ord a ⇒ Ord (ParamF a)
deriving stock instance Show a ⇒ Show (ParamF a)

data VarF ann
  = VarName ann Name
  | VarIndex ann (ExpF ann) (ExpF ann)
  | VarField ann (ExpF ann) Name

type Var = VarF Ann

deriving stock instance Eq a ⇒ Eq (VarF a)
deriving stock instance Ord a ⇒ Ord (VarF a)
deriving stock instance Show a ⇒ Show (VarF a)

data TableRowF ann
  = TableRowKV ann (ExpF ann) (ExpF ann)
  | TableRowNV ann Name (ExpF ann)

type TableRow = TableRowF Ann

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

newtype Ann = Ann ()
  deriving stock (Eq, Ord, Show)

newAnn ∷ Ann
newAnn = Ann ()

annL ∷ ∀ f a. HasAnn f ⇒ Lens' (f a) a
annL = lens annOf setAnn

class HasAnn f where
  annOf ∷ f a → a
  setAnn ∷ f a → a → f a

instance HasAnn VarF where
  annOf = \case
    VarName a _ → a
    VarIndex a _ _ → a
    VarField a _ _ → a
  setAnn f a = case f of
    VarName _ n → VarName a n
    VarIndex _ e1 e2 → VarIndex a e1 e2
    VarField _ e n → VarField a e n

instance HasAnn ParamF where
  annOf = \case
    ParamNamed a _ → a
    ParamUnused a → a
  setAnn f a = case f of
    ParamNamed _ n → ParamNamed a n
    ParamUnused _ → ParamUnused a

instance HasAnn TableRowF where
  annOf = \case
    TableRowKV a _ _ → a
    TableRowNV a _ _ → a
  setAnn f a = case f of
    TableRowKV _ k v → TableRowKV a k v
    TableRowNV _ n e → TableRowNV a n e

instance HasAnn ExpF where
  annOf = \case
    Nil a → a
    Boolean a _ → a
    Integer a _ → a
    Float a _ → a
    String a _ → a
    Function a _ _ → a
    TableCtor a _ → a
    UnOp a _ _ → a
    BinOp a _ _ _ → a
    Var a _ → a
    FunctionCall a _ _ → a
    ForeignSourceExp a _ → a
  setAnn expr a = case expr of
    Nil _ → Nil a
    Boolean _ b → Boolean a b
    Integer _ i → Integer a i
    Float _ f → Float a f
    String _ s → String a s
    Function _ p s → Function a p s
    TableCtor _ r → TableCtor a r
    UnOp _ o e → UnOp a o e
    BinOp _ o e1 e2 → BinOp a o e1 e2
    Var _ v → Var a v
    FunctionCall _ f args → FunctionCall a f args
    ForeignSourceExp _ src → ForeignSourceExp a src

instance HasAnn StatementF where
  annOf = \case
    Assign a _ _ → a
    Local a _ _ → a
    IfThenElse a _ _ _ → a
    Return a _ → a
    ForeignSourceStat a _ → a
  setAnn f a = case f of
    Assign _ v e → Assign a v e
    Local _ n e → Local a n e
    IfThenElse _ p t e → IfThenElse a p t e
    Return _ e → Return a e
    ForeignSourceStat _ src → ForeignSourceStat a src

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
  = Nil ann
  | Boolean ann Bool
  | Integer ann Integer
  | Float ann Double
  | String ann Text
  | Function ann [ParamF ann] [StatementF ann]
  | TableCtor ann [TableRowF ann]
  | UnOp ann UnaryOp (ExpF ann)
  | BinOp ann BinaryOp (ExpF ann) (ExpF ann)
  | Var ann (VarF ann)
  | FunctionCall ann (ExpF ann) [ExpF ann]
  | ForeignSourceExp ann Text

type Exp = ExpF Ann

deriving stock instance Eq a ⇒ Eq (ExpF a)
deriving stock instance Ord a ⇒ Ord (ExpF a)
deriving stock instance Show a ⇒ Show (ExpF a)

data StatementF ann
  = Assign ann (VarF ann) (ExpF ann)
  | Local ann Name (Maybe (ExpF ann))
  | IfThenElse
      ann
      (ExpF ann)
      -- ^ predicate
      [StatementF ann]
      -- ^ then block
      [StatementF ann]
      -- ^ else block
  | Return ann (ExpF ann)
  | ForeignSourceStat ann Text

type Statement = StatementF Ann

deriving stock instance Eq a ⇒ Eq (StatementF a)
deriving stock instance Ord a ⇒ Ord (StatementF a)
deriving stock instance Show a ⇒ Show (StatementF a)

--------------------------------------------------------------------------------
-- Smarter constructors --------------------------------------------------------

var ∷ Var → Exp
var = Var newAnn

assign ∷ Var → Exp → Statement
assign = Assign newAnn

assignVar ∷ Name → Exp → Statement
assignVar name = assign (VarName newAnn name)

local ∷ Name → Maybe Exp → Statement
local = Local newAnn

local1 ∷ Name → Exp → Statement
local1 name expr = Local newAnn name (Just expr)

local0 ∷ Name → Statement
local0 name = Local newAnn name Nothing

ifThenElse ∷ Exp → [Statement] → [Statement] → Statement
ifThenElse = IfThenElse newAnn

return ∷ Exp → Statement
return = Return newAnn

foreignStatement ∷ Text → Statement
foreignStatement = ForeignSourceStat newAnn

chunkToExpression ∷ Chunk → Exp
chunkToExpression = scope . toList

-- Expressions -----------------------------------------------------------------

nil ∷ Exp
nil = Nil newAnn

boolean ∷ Bool → Exp
boolean = Boolean newAnn

integer ∷ Integer → Exp
integer = Integer newAnn

float ∷ Double → Exp
float = Float newAnn

string ∷ Text → Exp
string = String newAnn

table ∷ [TableRow] → Exp
table = TableCtor newAnn

functionDef ∷ [Param] → [Statement] → Exp
functionDef = Function newAnn

functionCall ∷ Exp → [Exp] → Exp
functionCall = FunctionCall newAnn

foreignExpression ∷ Text → Exp
foreignExpression = ForeignSourceExp newAnn

unOp ∷ UnaryOp → Exp → Exp
unOp = UnOp newAnn

binOp ∷ BinaryOp → Exp → Exp → Exp
binOp = BinOp newAnn

error ∷ Text → Exp
error msg = functionCall (varName [Lua.name|error|]) [String newAnn msg]

pun ∷ Name → TableRow
pun n = TableRowNV newAnn n (varName n)

thunk ∷ Exp → Exp
thunk e = scope [return e]

scope ∷ [Statement] → Exp
scope body = functionCall (functionDef [] body) []

-- Unary operators -------------------------------------------------------------

hash ∷ Exp → Exp
hash = UnOp newAnn HashOp

negate ∷ Exp → Exp
negate = UnOp newAnn Negate

logicalNot ∷ Exp → Exp
logicalNot = UnOp newAnn LogicalNot

bitwiseNot ∷ Exp → Exp
bitwiseNot = UnOp newAnn BitwiseNot

-- Binary operators ------------------------------------------------------------

or ∷ Exp → Exp → Exp
or = BinOp newAnn Or

and ∷ Exp → Exp → Exp
and = BinOp newAnn And

lessThan ∷ Exp → Exp → Exp
lessThan = BinOp newAnn LessThan

greaterThan ∷ Exp → Exp → Exp
greaterThan = BinOp newAnn GreaterThan

lessThanOrEqualTo ∷ Exp → Exp → Exp
lessThanOrEqualTo = BinOp newAnn LessThanOrEqualTo

greaterThanOrEqualTo ∷ Exp → Exp → Exp
greaterThanOrEqualTo = BinOp newAnn GreaterThanOrEqualTo

notEqualTo ∷ Exp → Exp → Exp
notEqualTo = BinOp newAnn NotEqualTo

equalTo ∷ Exp → Exp → Exp
equalTo = BinOp newAnn EqualTo

bitOr ∷ Exp → Exp → Exp
bitOr = BinOp newAnn BitOr

bitXor ∷ Exp → Exp → Exp
bitXor = BinOp newAnn BitXor

bitAnd ∷ Exp → Exp → Exp
bitAnd = BinOp newAnn BitAnd

bitShiftRight ∷ Exp → Exp → Exp
bitShiftRight = BinOp newAnn BitShiftRight

bitShiftLeft ∷ Exp → Exp → Exp
bitShiftLeft = BinOp newAnn BitShiftLeft

concat ∷ Exp → Exp → Exp
concat = BinOp newAnn Concat

add ∷ Exp → Exp → Exp
add = BinOp newAnn Add

sub ∷ Exp → Exp → Exp
sub = BinOp newAnn Sub

mul ∷ Exp → Exp → Exp
mul = BinOp newAnn Mul

floatDiv ∷ Exp → Exp → Exp
floatDiv = BinOp newAnn FloatDiv

floorDiv ∷ Exp → Exp → Exp
floorDiv = BinOp newAnn FloorDiv

mod ∷ Exp → Exp → Exp
mod = BinOp newAnn Mod

exponent ∷ Exp → Exp → Exp
exponent = BinOp newAnn Exp

-- Table Rows ------------------------------------------------------------------

tableRowKV ∷ Exp → Exp → TableRow
tableRowKV = TableRowKV newAnn

tableRowNV ∷ Name → Exp → TableRow
tableRowNV = TableRowNV newAnn

-- Params ----------------------------------------------------------------------

paramNamed ∷ Name → Param
paramNamed = ParamNamed newAnn

paramUnused ∷ Param
paramUnused = ParamUnused newAnn

-- Variables -------------------------------------------------------------------

varName ∷ Name → Exp
varName = var . VarName newAnn

varIndex ∷ Exp → Exp → Exp
varIndex = (var .) . VarIndex newAnn

varField ∷ Exp → Name → Exp
varField = (var .) . VarField newAnn
